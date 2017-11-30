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
import           Data.Monoid               ((<>))
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
import           Kurokos.GUI.WidgetTree    (WidgetTree (..))
import qualified Kurokos.GUI.WidgetTree    as WT
import qualified Kurokos.RPN               as RPN

data TextureInfo = TextureInfo
  { _tiPos  :: GuiPos
  , _tiSize :: GuiSize
  }

makeLenses ''TextureInfo

data WContext = WContext
  { _ctxKey         :: WTKey
  , _ctxIdent       :: Maybe WidgetIdent
  , _ctxNeedsRender :: Bool
  , _ctxWidgetState :: WidgetState
  , _ctxColorSet    :: ColorSet
  , _ctxColor       :: WidgetColor
  , _ctxTexture     :: SDL.Texture
  , _ctxTextureInfo :: TextureInfo
  , _ctxUPos        :: V2 Exp
  , _ctxUSize       :: V2 Exp
  }

makeLenses ''WContext

type GuiWidgetTree = WidgetTree (WContext, Widget)

mapWTPos :: (GuiPos -> (WContext, Widget) -> a) -> GuiPos -> GuiWidgetTree -> WidgetTree a
mapWTPos f = work
  where
    work _   Null                = Null
    work pos (Single u a o)      = Single (work pos u) (f pos a) (work pos o)
    work pos (Container u a c o) = Container (work pos u) (f pos' a) (work pos' c) (work pos o)
      where
        ti = a^._1.ctxTextureInfo
        pos' = pos + ti^.tiPos

data GuiEnv = GuiEnv
  { geFont            :: Font.Font
  , geDefaultColorSet :: ColorSet
  }

data GUI = GUI
  { _gKeyCnt         :: Key
  , _gWTree          :: GuiWidgetTree
  , _gEvents         :: [GuiEvent]
  , _gDragTrajectory :: [Point V2 Int32]
  }

makeLenses ''GUI

iniGui :: GUI
iniGui = GUI 0 Null [] []

getWidgetTrees :: GUI -> WidgetTree Widget
getWidgetTrees = fmap snd . view gWTree

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
  readyRender gui

genSingle :: (RenderEnv m, MonadIO m, E.MonadThrow m)
  => Maybe WidgetIdent -> V2 UExp -> V2 UExp -> Widget -> GuiT m GuiWidgetTree
genSingle mName pos size w = do
  key <- WTKey <$> use gKeyCnt
  gKeyCnt += 1
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  colset <- asks geDefaultColorSet
  let ti = TextureInfo (pure 0) (pure 1)
  tex <- lift $ withRenderer $ \r ->
    SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget (pure 1)
  let ctx = WContext key mName True iniWidgetState colset (colorSetBasis colset) tex ti pos' size'
  return $ Single Null (ctx,w) Null

genContainer :: (RenderEnv m, MonadIO m, E.MonadThrow m)
  => V2 UExp -> V2 UExp -> GuiT m GuiWidgetTree
genContainer pos size = do
  key <- WTKey <$> use gKeyCnt
  gKeyCnt += 1
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  colset <- asks geDefaultColorSet
  tex <- lift $ withRenderer $ \r ->
    SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget (pure 1)
  let ctx = WContext key Nothing True iniWidgetState colset (colorSetBasis colset) tex ti pos' size'
      w = Fill
  return $ Container Null (ctx,w) Null Null
  where
    ti = TextureInfo (pure 0) (pure 1)

appendRoot :: Monad m => GuiWidgetTree -> GuiT m ()
appendRoot wt = modify $ over gWTree (wt <>)

prependRoot :: Monad m => GuiWidgetTree -> GuiT m ()
prependRoot wt = modify $ over gWTree (<> wt)

-- Rendering GUI

setAllNeedsRender :: GUI -> GUI
setAllNeedsRender =
  over gWTree (fmap setNR)
  where
    setNR = set (_1 . ctxNeedsRender) True

readyRender :: (RenderEnv m, MonadIO m, MonadMask m) => GUI -> m GUI
readyRender g = do
  V2 w h <- getWindowSize
  let vmap = M.fromList
        [ (keyWidth, w)
        , (keyHeight, h)
        , (keyWinWidth, w)
        , (keyWinHeight, h)]
  wt <- go vmap $ g^.gWTree
  return $ g&gWTree .~ wt
  where
    makeTexture vmap (ctx, widget) = do
      let vmap' = M.map fromIntegral vmap
      pos <- case evalExp2 vmap' upos of
              Left err -> E.throw $ userError err
              Right v  -> return $ P v
      size <- case evalExp2 vmap' usize of
              Left err -> E.throw $ userError err
              Right v  -> return v
      tex <- createTexture' size wcol widget renderWidget
      let ctx' = ctx & ctxNeedsRender .~ False
                     & ctxTexture .~ tex
                     & ctxTextureInfo .~ TextureInfo pos size
      return (ctx', widget)
      where
        upos = ctx^.ctxUPos
        usize = ctx^.ctxUSize
        wcol = ctx^.ctxColor

    go _ Null = return Null
    go vmap (Single u a o) = do
      u' <- go vmap u
      o' <- go vmap o
      if ctx^.ctxNeedsRender
        then do
          SDL.destroyTexture $ ctx^.ctxTexture
          a' <- makeTexture vmap a
          return $ Single u' a' o'
        else return $ Single u' a o'
      where
        ctx = a^._1
    go vmap (Container u a c o) = do
      a' <- if ctx^.ctxNeedsRender
              then do
                SDL.destroyTexture $ ctx^.ctxTexture
                makeTexture vmap a
              else return a
      u' <- go vmap u
      o' <- go vmap o
      let (V2 w h) = fromIntegral <$> (a'^._1 . ctxTextureInfo . tiSize)
          vmap' = M.insert keyWidth w . M.insert keyHeight h $ vmap -- Update width and height
      c' <- go vmap' c
      return $ Container u' a' c' o'
      where
        ctx = a^._1

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

render :: (RenderEnv m, MonadIO m, MonadMask m) => GUI -> m ()
render = go (pure 0) . view gWTree
  where
    go _ Null = return ()
    go pos0 (Single u (ctx,_) o)
      | ctx^.ctxNeedsRender = E.throw $ userError "Call GUI.readyRender before GUI.render!"
      | otherwise = do
        go pos0 u
        renderTexture (ctx^.ctxTexture) $ Rectangle pos' (ti^.tiSize)
        go pos0 o
        where
          ti = ctx^.ctxTextureInfo
          pos' = pos0 + (ti^.tiPos)
    go pos0 (Container u (ctx,_) c o)
      | ctx^.ctxNeedsRender = E.throw $ userError "Call GUI.readyRender before GUI.render!"
      | otherwise = do
        go pos0 u
        renderTexture (ctx^.ctxTexture) $ Rectangle pos' (ti^.tiSize)
        go pos' c
        go pos0 o
        where
          ti = ctx^.ctxTextureInfo
          pos' = pos0 + (ti^.tiPos)
