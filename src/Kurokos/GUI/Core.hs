{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Kurokos.GUI.Core where

import           Control.Concurrent.MVar
import qualified Control.Exception.Safe    as E
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Int                  (Int64)
import qualified Data.Map                  as M
import           Data.Text                 (Text)

import           SDL                       (($=))
import qualified SDL
import qualified SDL.Font                  as Font

import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget
import           Kurokos.GUI.Widget.Render
import qualified Kurokos.RPN               as RPN

type Key = Int64

newtype SingleKey = SingleKey Key deriving Show
newtype ContainerKey = ContainerKey Key deriving Show

data TextureInfo = TextureInfo
  { tiTexture :: SDL.Texture
  , tiPos     :: GuiPos
  , tiSize    :: GuiSize
  }

data WidgetTree
  = Single
      { singleKey :: SingleKey
      , wtColor   :: WidgetColor
      , wtTexture :: MVar TextureInfo
      , wtUPos    :: V2 Exp
      , wtUSize   :: V2 Exp
      , wtWidget  :: Widget
      }
  | Container
      { containerKey :: ContainerKey
      , wtTexture    :: MVar TextureInfo
      , wtUPos       :: V2 Exp
      , wtUSize      :: V2 Exp
      , wtChildren   :: [WidgetTree]
      }

instance Show WidgetTree where
  show Single{..} = show key ++ show wtWidget
    where (SingleKey key) = singleKey
  show Container{..} = show key ++ show wtChildren
    where (ContainerKey key) = containerKey

data GuiEnv = GuiEnv
  { geFont :: Font.Font
  , geDefaultWidgetColor :: WidgetColor
  }

data GUI = GUI
  { _gSCnt  :: Key
  , _gCCnt  :: Key
  --
  , _gWTrees :: [WidgetTree]
  } deriving Show

makeLenses ''GUI

getWidgetTrees :: GUI -> [WidgetTree]
getWidgetTrees g = g^.gWTrees

newtype GuiT m a = GuiT {
    runGT :: ReaderT GuiEnv (StateT GUI m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader GuiEnv, MonadState GUI, E.MonadThrow)

runGuiT :: Monad m => GuiEnv -> GUI -> GuiT m a -> m GUI
runGuiT env g k = execStateT (runReaderT (runGT k) env) g

instance MonadTrans GuiT where
  lift = GuiT . lift . lift

newGui :: (RenderEnv m, MonadIO m, MonadMask m, MonadThrow m)
  => GuiEnv -> GuiT m () -> m GUI
newGui env initializer = do
  gui <- runGuiT env (GUI 0 0 []) initializer
  updateTexture gui
  return gui

genSingle :: (MonadIO m, E.MonadThrow m)
  => V2 UExp -> V2 UExp -> Widget -> GuiT m WidgetTree
genSingle pos size w = do
  key <- SingleKey <$> use gSCnt
  gSCnt += 1
  mti <- liftIO newEmptyMVar
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  wc <- asks geDefaultWidgetColor
  return $ Single key wc mti pos' size' w

genContainer :: (MonadIO m, E.MonadThrow m)
  => V2 UExp -> V2 UExp -> [WidgetTree] -> GuiT m WidgetTree
genContainer pos size ws = do
  key <- ContainerKey <$> use gCCnt
  gCCnt += 1
  mti <- liftIO newEmptyMVar
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  return $ Container key mti pos' size' ws

prependRoot :: Monad m => WidgetTree -> GuiT m ()
prependRoot w = modify $ over gWTrees (w:)

prependRootWs :: Monad m => [WidgetTree] -> GuiT m ()
prependRootWs ws = modify $ over gWTrees (ws ++)

-- Rendering GUI

resetTexture :: MonadIO m => GUI -> m ()
resetTexture = liftIO . mapM_ go . view gWTrees
  where
    go Single{..} = do
      p <- isEmptyMVar wtTexture
      unless p $ void $ takeMVar wtTexture
    go Container{..} = do
      p <- isEmptyMVar wtTexture
      unless p $ void $ takeMVar wtTexture
      mapM_ go wtChildren

updateTexture :: (RenderEnv m, MonadIO m, MonadMask m) => GUI -> m ()
updateTexture g = do
  V2 w h <- getWindowSize
  let vmap = M.fromList
        [ (keyWidth, w)
        , (keyHeight, h)
        , (keyWinWidth, w)
        , (keyWinHeight, h)]
  mapM_ (go vmap) (g^.gWTrees)
  where
    makeTextureInfo vmap upos usize mWidget = do
      let vmap' = M.map fromIntegral vmap
      pos <- case evalExp2 vmap' upos of
              Left err -> E.throw $ userError err
              Right v  -> return v
      size <- case evalExp2 vmap' usize of
              Left err -> E.throw $ userError err
              Right v  -> return v
      tex <- case mWidget of
        Just (widget, wcol) -> createTexture' size wcol widget renderWidget
        Nothing -> createDummyTexture size
      return $ TextureInfo tex pos size

    go vmap Single{..} = do
      pEmpty <- liftIO $ isEmptyMVar wtTexture
      when pEmpty $ do
        ti@TextureInfo{..} <- makeTextureInfo vmap wtUPos wtUSize (Just (wtWidget, wtColor))
        liftIO $ putMVar wtTexture ti
    go vmap Container{..} = do
      pEmpty <- liftIO $ isEmptyMVar wtTexture
      when pEmpty $ do
        ti@TextureInfo{..} <- makeTextureInfo vmap wtUPos wtUSize Nothing
        liftIO $ putMVar wtTexture ti
      mapM_ (go vmap) wtChildren

    evalExp2 :: M.Map String Double -> V2 Exp -> Either String (V2 CInt)
    evalExp2 vmap (V2 x y) = V2 <$> evalExp x <*> evalExp y
      where
        evalExp (ERPN expr) = truncate <$> RPN.eval vmap expr
        evalExp (EConst v)  = return v

    createTexture' size wcol w renderW =
      withRenderer $ \r -> do
        tex <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget size
        SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
        E.bracket_ (SDL.rendererRenderTarget r $= Just tex)
                   (SDL.rendererRenderTarget r $= Nothing)
                   (renderW r size wcol w)
        return tex

    createDummyTexture size =
      withRenderer $ \r -> do
        tex <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget size
        SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
        return tex

render :: (RenderEnv m, MonadIO m, MonadMask m) => GUI -> m ()
render = mapM_ (go (pure 0)) . view gWTrees
  where
    go pos0 Single{..} = do
      pEmpty <- liftIO $ isEmptyMVar wtTexture
      unless pEmpty $ do
        TextureInfo{..} <- liftIO $ readMVar wtTexture
        let pos' = SDL.P $ pos0 + tiPos
        renderTexture tiTexture $ SDL.Rectangle pos' tiSize
    go pos0 Container{..} = do
      pEmpty <- liftIO $ isEmptyMVar wtTexture
      unless pEmpty $ do
        TextureInfo{..} <- liftIO $ readMVar wtTexture
        let pos' = pos0 + tiPos
        mapM_ (go pos') wtChildren
