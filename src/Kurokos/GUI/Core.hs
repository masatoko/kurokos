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
import qualified Data.Map                  as M
import           Data.Text                 (Text)
import           Linear.V2

import           SDL                       (($=))
import qualified SDL
import qualified SDL.Font                  as Font

import           Kurokos.GUI.Event         (GuiEvent)
import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget
import           Kurokos.GUI.Widget.Render
import qualified Kurokos.RPN               as RPN

data TextureInfo = TextureInfo
  { tiPos  :: GuiPos
  , tiSize :: GuiSize
  }

data WidgetTree
  = Single
      { wtKey     :: WTKey
      , wtName    :: Maybe String
      , wtColor   :: WidgetColor
      , wtTexture :: MVar SDL.Texture
      , wtTexInfo :: TextureInfo
      , wtUPos    :: V2 Exp
      , wtUSize   :: V2 Exp
      , wtWidget  :: Widget
      }
  | Container
      { wtKey      :: WTKey
      , wtTexture  :: MVar SDL.Texture
      , wtTexInfo  :: TextureInfo
      , wtUPos     :: V2 Exp
      , wtUSize    :: V2 Exp
      , wtChildren :: [WidgetTree]
      }

instance Show WidgetTree where
  show Single{..} = show key ++ show wtWidget
    where (WTKey key) = wtKey
  show Container{..} = show key ++ show wtChildren
    where (WTKey key) = wtKey

-- foldWT :: (WidgetTree -> a -> a) -> a -> WidgetTree -> a
-- foldWT f a s@Single{}    = f s a
-- foldWT f a Container{..} = foldr f a wtChildren

data GuiEnv = GuiEnv
  { geFont               :: Font.Font
  , geDefaultWidgetColor :: WidgetColor
  }

data GUI = GUI
  { _gKeyCnt :: Key
  --
  , _gWTrees :: [WidgetTree]
  --
  , _gEvents :: [GuiEvent]
  } deriving Show

makeLenses ''GUI

iniGui :: GUI
iniGui = GUI 0 [] []

getWidgetTrees :: GUI -> [WidgetTree]
getWidgetTrees = view gWTrees

getGuiEvents :: GUI -> [GuiEvent]
getGuiEvents = view gEvents

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
  gui <- runGuiT env iniGui initializer
  updateTexture gui

genSingle :: (MonadIO m, E.MonadThrow m)
  => Maybe String -> V2 UExp -> V2 UExp -> Widget -> GuiT m WidgetTree
genSingle mName pos size w = do
  key <- WTKey <$> use gKeyCnt
  gKeyCnt += 1
  mTex <- liftIO newEmptyMVar
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  wc <- asks geDefaultWidgetColor
  let ti = TextureInfo (pure 0) (pure 0)
  return $ Single key mName wc mTex ti pos' size' w

genContainer :: (MonadIO m, E.MonadThrow m)
  => V2 UExp -> V2 UExp -> [WidgetTree] -> GuiT m WidgetTree
genContainer pos size ws = do
  key <- WTKey <$> use gKeyCnt
  gKeyCnt += 1
  mTex <- liftIO newEmptyMVar
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  let ti = TextureInfo (pure 0) (pure 0)
  return $ Container key mTex ti pos' size' ws

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

updateTexture :: (RenderEnv m, MonadIO m, MonadMask m) => GUI -> m GUI
updateTexture g = do
  V2 w h <- getWindowSize
  let vmap = M.fromList
        [ (keyWidth, w)
        , (keyHeight, h)
        , (keyWinWidth, w)
        , (keyWinHeight, h)]
  wts <- mapM (go vmap) (g^.gWTrees)
  return $ g&gWTrees .~ wts
  where
    makeTexture vmap upos usize mWidget = do
      liftIO $ putStrLn "====="
      let vmap' = M.map fromIntegral vmap
      liftIO $ do
        print vmap
        print vmap'
        print upos
        print usize
      pos <- case evalExp2 vmap' upos of
              Left err -> E.throw $ userError err
              Right v  -> return $ P v
      size <- case evalExp2 vmap' usize of
              Left err -> E.throw $ userError err
              Right v  -> return v
      liftIO . print $ (pos, size, mWidget)
      tex <- case mWidget of
        Just (widget, wcol) -> createTexture' size wcol widget renderWidget
        Nothing             -> createDummyTexture size
      return (tex, TextureInfo pos size)

    go vmap wt@Single{..} = do
      pEmpty <- liftIO $ isEmptyMVar wtTexture
      if pEmpty
        then do
          (tex, ti) <- makeTexture vmap wtUPos wtUSize (Just (wtWidget, wtColor))
          liftIO $ putMVar wtTexture tex
          return $ wt {wtTexInfo = ti}
        else return wt
    go vmap wt@Container{..} = do
      pEmpty <- liftIO $ isEmptyMVar wtTexture
      (wt', vmap') <- if pEmpty
        then do
          (tex, ti) <- makeTexture vmap wtUPos wtUSize Nothing
          liftIO $ putMVar wtTexture tex
          let (V2 w h) = fromIntegral <$> tiSize ti
              vmap' = M.insert keyWidth w . M.insert keyHeight h $ vmap -- Update width and height
          return (wt {wtTexInfo = ti}, vmap')
        else return (wt, vmap)
      ws <- mapM (go vmap') wtChildren
      return $ wt' {wtChildren = ws}

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
        let TextureInfo{..} = wtTexInfo
        tex <- liftIO $ readMVar wtTexture
        let pos' = pos0 + tiPos
        renderTexture tex $ Rectangle pos' tiSize
    go pos0 Container{..} = do
      pEmpty <- liftIO $ isEmptyMVar wtTexture
      unless pEmpty $ do
        let TextureInfo{..} = wtTexInfo
        -- tex <- liftIO $ readMVar wtTexture
        let pos' = pos0 + tiPos
        mapM_ (go pos') wtChildren
