{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Kurokos.GUI.Core where

import           Control.Exception.Safe    (MonadMask)
import qualified Control.Exception.Safe    as E
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Int                  (Int64)
import qualified Data.Map                  as M
import           Data.Text                 (Text)
import           Foreign.C.Types           (CInt)
import           Linear.V2

import           SDL                       (($=))
import qualified SDL
import qualified SDL.Font                  as Font

import           Kurokos.GUI.Def           (RenderEnv (..))
import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget
import           Kurokos.GUI.Widget.Render
import qualified Kurokos.RPN as RPN

-- class Widget a where
--   showW :: a -> String
--   render :: (MonadIO m, MonadMask m) => RenderEnv m => a -> m ()

type Key = Int64
newtype SingleKey = SingleKey Key deriving Show
newtype ContainerKey = ContainerKey Key deriving Show

data TextureInfo = TextureInfo
  { tiTexture :: SDL.Texture
  , tiPos :: GuiPos
  , tiSize :: GuiSize
  }

data WidgetTree
  = Single
      { singleKey :: SingleKey
      , wtTexture :: Maybe TextureInfo
      , wtUPos    :: V2 Exp
      , wtUSize   :: V2 Exp
      , wtWidget  :: Widget
      }
  | Container
      { containerKey :: ContainerKey
      , wtTexture    :: Maybe TextureInfo
      , wtUPos       :: V2 Exp
      , wtUSize      :: V2 Exp
      , wtChildren   :: [WidgetTree]
      }

instance Show WidgetTree where
  show Single{..} = show key ++ show wtWidget
    where (SingleKey key) = singleKey
  show Container{..} = show key ++ show wtChildren
    where (ContainerKey key) = containerKey

newtype GuiEnv = GuiEnv
  { geFont :: Font.Font
  }

data GUI = GUI
  { _gSCnt  :: Key
  , _gCCnt  :: Key
  --
  , _gWTree :: WidgetTree
  } deriving Show

makeLenses ''GUI

getWidgetTree :: GUI -> WidgetTree
getWidgetTree = _gWTree

newtype GuiT m a = GuiT {
    runGT :: ReaderT GuiEnv (StateT GUI m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader GuiEnv, MonadState GUI, E.MonadThrow)

runGuiT :: Monad m => GuiEnv -> GUI -> GuiT m a -> m GUI
runGuiT env g k = execStateT (runReaderT (runGT k) env) g

instance MonadTrans GuiT where
  lift = GuiT . lift . lift

newGui :: (RenderEnv m, MonadIO m, E.MonadThrow m) => GuiEnv -> GuiT m () -> m GUI
newGui env initializer = do
  (V2 w h) <- getWindowSize
  let pos = pure (EConst 0)
      size = V2 (EConst w) (EConst h)
      gui = GUI 0 1 (Container (ContainerKey 0) Nothing pos size [])
  runGuiT env gui initializer

genSingle :: E.MonadThrow m
  => V2 UExp -> V2 UExp -> Widget -> GuiT m WidgetTree
genSingle pos size w = do
  key <- SingleKey <$> use gSCnt
  gSCnt += 1
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  return $ Single key Nothing pos' size' w

genContainer :: E.MonadThrow m
  => V2 UExp -> V2 UExp -> [WidgetTree] -> GuiT m WidgetTree
genContainer pos size ws = do
  key <- ContainerKey <$> use gCCnt
  gCCnt += 1
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  return $ Container key Nothing pos' size' ws

putWT :: Monad m => WidgetTree -> GuiT m ()
putWT wt = gWTree .= wt

renderGUI :: (RenderEnv m, MonadIO m, MonadMask m) => GUI -> m GUI
renderGUI g = do
  ws <- go (pure 0) (g^.gWTree)
  return $ g & gWTree .~ ws
  where
    makeTextureInfo upos usize mWidget = do
      let vmap = M.empty
      pos <- case evalExp2 vmap upos of
              Left err -> E.throw $ userError err
              Right v  -> return v
      size <- case evalExp2 vmap usize of
              Left err -> E.throw $ userError err
              Right v  -> return v
      tex <- case mWidget of
        Just widget -> createTexture' size widget renderWidget
        Nothing     -> createDummyTexture size
      return $ TextureInfo tex pos size

    go pos0 wt@Single{..} =
      case wtTexture of
        Nothing -> do
          ti@TextureInfo{..} <- makeTextureInfo wtUPos wtUSize (Just wtWidget)
          let pos' = SDL.P $ pos0 + tiPos
          renderTexture tiTexture $ SDL.Rectangle pos' tiSize
          return $ wt {wtTexture = Just ti}
        Just TextureInfo{..} -> do
          let pos' = SDL.P $ pos0 + tiPos
          renderTexture tiTexture $ SDL.Rectangle pos' tiSize
          return wt
    go pos0 wt@Container{..} =
      case wtTexture of
        Nothing -> do
          ti@TextureInfo{..} <- makeTextureInfo wtUPos wtUSize Nothing
          let pos' = pos0 + tiPos
          ws <- mapM (go pos') wtChildren
          return $ wt {wtTexture = Just ti, wtChildren = ws}
        Just TextureInfo{..} -> do
          let pos' = pos0 + tiPos
          ws <- mapM (go pos') wtChildren
          return $ wt {wtChildren = ws}

evalExp2 :: M.Map String Double -> V2 Exp -> Either String (V2 CInt)
evalExp2 vmap (V2 x y) = V2 <$> evalExp x <*> evalExp y
  where
    evalExp (ERPN expr) = truncate <$> RPN.eval vmap expr
    evalExp (EConst v)  = return v

createTexture' size w render =
  withRenderer $ \r -> do
    tex <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget size
    SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
    E.bracket_ (SDL.rendererRenderTarget r $= Just tex)
               (SDL.rendererRenderTarget r $= Nothing)
               (render r size w)
    return tex

createDummyTexture size =
  withRenderer $ \r -> do
    tex <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget size
    SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
    return tex
