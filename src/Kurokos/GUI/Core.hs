{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import           Data.Maybe                (fromMaybe)
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

type CtxWidget = (WContext, Widget)
type GuiWidgetTree = WidgetTree CtxWidget

-- Update visibiilty in WidgetState
updateVisibility :: GuiWidgetTree -> GuiWidgetTree
updateVisibility = work True
  where
    work _    Null            = Null
    work vis0 (Fork u a mc o) =
      Fork (work vis0 u) a' (work vis' <$> mc) (work vis0 o)
      where
        atr = a^._1.ctxAttrib -- Original attribute
        vis' = vis0 && atr^.visible -- Current state
        a' = a & _1 . ctxWidgetState . wstVisible .~ vis'

-- Update global position in WidgetState
updateLayout :: GuiWidgetTree -> GuiWidgetTree
updateLayout wt0 = fst $ work wt0 Unordered (P $ V2 0 0)
  where
    modsize ct (x,p) = do
      case ct of
        Unordered       -> return ()
        VerticalStack   -> _y .= (p^._y)
        HorizontalStack -> _x .= (p^._x)
      return x

    work Null            _   p0 = (Null, p0)
    work (Fork u a mc o) ct0 p0 = runState go p0
      where
        wst = a^._1.ctxWidgetState
        ct' = fromMaybe Unordered $ a^._1.ctxContainerType
        go = do
          -- Under
          u' <- modsize ct0 . work u ct0 =<< get
          -- CtxWidget
          pos <- get
          let pos' = case ct0 of
                      Unordered -> p0 + (wst^.wstPos)
                      _         -> pos
              a' = a & _1 . ctxWidgetState . wstGlobalPos .~ pos'
          modsize ct0 ((), pos' + P (wst^.wstSize))
          -- Children
          mc' <- case mc of
            Nothing -> return Nothing
            Just c  -> fmap Just $ modsize ct0 $ work c ct' pos'
          -- Over
          o' <- modsize ct0 . work o ct0 =<< get
          return $ Fork u' a' mc' o'

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

getWidgetTree :: GUI -> WidgetTree Widget
getWidgetTree = fmap snd . view gWTree

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
newGui env initializer =
  readyRender . over gWTree WT.balance =<< runGuiT env iniGui initializer

genSingle :: (RenderEnv m, MonadIO m, E.MonadThrow m)
  => Maybe WidgetIdent -> V2 UExp -> V2 UExp -> Widget -> GuiT m GuiWidgetTree
genSingle mIdent pos size w = do
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
  let ctx = WContext key mIdent Nothing (attribOf w) True iniWidgetState colset (colorSetBasis colset) tex pos' size'
  return $ Fork Null (ctx, w) Nothing Null

genContainer :: (RenderEnv m, MonadIO m, E.MonadThrow m)
  => Maybe WidgetIdent -> ContainerType -> V2 UExp -> V2 UExp -> GuiT m GuiWidgetTree
genContainer mIdent ct pos size = do
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
  let w = Transparent
      ctx = WContext key mIdent (Just ct) (attribOf w) True iniWidgetState colset (colorSetBasis colset) tex pos' size'
  return $ Fork Null (ctx,w) (Just Null) Null

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
  return $ g & gWTree .~ (updateLayout . updateVisibility) wt
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
                     & ctxWidgetState . wstPos .~ pos
                     & ctxWidgetState . wstSize .~ size
      return (ctx', widget)
      where
        upos = ctx^.ctxUPos
        usize = ctx^.ctxUSize
        wcol = ctx^.ctxColor

    go _ Null = return Null
    go vmap (Fork u a Nothing o) = do
      u' <- go vmap u
      o' <- go vmap o
      if ctx^.ctxNeedsRender
        then do
          SDL.destroyTexture $ ctx^.ctxTexture
          a' <- makeTexture vmap a
          return $ Fork u' a' Nothing o'
        else return $ Fork u' a Nothing o'
      where
        ctx = a^._1
    go vmap (Fork u a (Just c) o) = do
      a' <- if ctx^.ctxNeedsRender
              then do
                SDL.destroyTexture $ ctx^.ctxTexture
                makeTexture vmap a
              else return a
      u' <- go vmap u
      o' <- go vmap o
      let (V2 w h) = fromIntegral <$> (a'^._1 . ctxWidgetState . wstSize)
          vmap' = M.insert keyWidth w . M.insert keyHeight h $ vmap -- Update width and height
      c' <- go vmap' c
      return $ Fork u' a' (Just c') o'
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
render = mapM_ go . view gWTree
  where
    go (ctx,_)
      | ctx^.ctxNeedsRender            = E.throw $ userError "Call GUI.readyRender before GUI.render!"
      | ctx^.ctxWidgetState.wstVisible = renderTexture (ctx^.ctxTexture) rect
      | otherwise                      = return ()
        where
          rect = Rectangle pos size
          pos = ctx^.ctxWidgetState.wstGlobalPos
          size = ctx^.ctxWidgetState^.wstSize
