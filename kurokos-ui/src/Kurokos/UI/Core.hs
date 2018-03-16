{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Kurokos.UI.Core where

import           Control.Concurrent.MVar
import qualified Control.Exception        as E
import           Control.Lens
import           Control.Monad.Extra      (whenJust)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString          (ByteString)
import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe, isJust)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Linear.V2

import           SDL                      (($=))
import qualified SDL

import qualified Kurokos.Asset            as Asset
import qualified Kurokos.Asset.Raw        as Asset
import qualified Kurokos.RPN              as RPN

import           Kurokos.UI.Color
import           Kurokos.UI.Color.Scheme  (ColorScheme, lookupColorOfWidget)
import           Kurokos.UI.Event         (GuiEvent)
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import           Kurokos.UI.Widget.Render
import           Kurokos.UI.WidgetTree    (WidgetTree (..))
import qualified Kurokos.UI.WidgetTree    as WT
import qualified Kurokos.Graphics as G
import Kurokos.UI.Widget.Update (onReadyLayout)

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
updateLayout wt0 = fst $ work wt0 Unordered False (P $ V2 0 0)
  where
    help f (a,p) = f p >> return a
    modsize Unordered       _ = return ()
    modsize VerticalStack   p = _y .= (p^._y)
    modsize HorizontalStack p = _x .= (p^._x)

    work Null            _   _            p0 = (Null, p0)
    work (Fork u a mc o) ct0 parentLayout p0 = runState go p0
      where
        wst = a^._1.ctxWidgetState
        shouldLayout = parentLayout || (a^._1.ctxNeedsLayout)
        ct' = fromMaybe Unordered $ a^._1.ctxContainerType
        go = do
          -- Under
          u' <- help (modsize ct0) . work u ct0 parentLayout =<< get
          -- CtxWidget
          pos <- get
          let pos' = case ct0 of
                      Unordered -> p0 + (wst^.wstPos)
                      _         -> pos
              a' = if shouldLayout
                      then a & _1 . ctxWidgetState . wstGlobalPos .~ pos'
                             & _1 . ctxNeedsLayout .~ False
                      else a
          modsize ct0 $ pos' + P (wst^.wstSize)
          -- Children
          mc' <- case mc of
            Nothing -> return Nothing
            Just c  -> fmap Just $ help (modsize ct0) $ work c ct' shouldLayout pos'
          -- Over
          o' <- help (modsize ct0) . work o ct0 parentLayout =<< get
          return $ Fork u' a' mc' o'

data GuiEnv = GuiEnv
  { geAssetManager :: Asset.AssetManager
  , geColorScheme  :: ColorScheme
  }

data GuiState = GuiState
  { _gstIdCnt :: WidgetIdent
  -- ^ Counter for WidgetTree ID
  , _gstWTree :: GuiWidgetTree
  }

makeLenses ''GuiState

newtype GUI = GUI { _unGui :: (GuiEnv, GuiState) }

makeLenses ''GUI

getWidgetTree :: GUI -> WidgetTree Widget
getWidgetTree = fmap snd . view gstWTree . snd . _unGui

newtype GuiT m a = GuiT {
    runGT :: ReaderT GuiEnv (StateT GuiState m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader GuiEnv, MonadState GuiState)

runGuiT :: Monad m => GUI -> GuiT m a -> m GUI
runGuiT (GUI (env, gst)) k = do
  gst' <- execStateT (runReaderT (runGT k) env) gst
  return $ GUI (env, gst')

instance MonadTrans GuiT where
  lift = GuiT . lift . lift

newGui :: (RenderEnv m, MonadIO m)
  => GuiEnv -> GuiT m () -> m GUI
newGui env initializer = do
  g1 <- runGuiT g0 initializer
  return $ g1 & unGui._2.gstWTree %~ WT.balance
  where
    g0 = GUI (env, gst0)
    gst0 = GuiState 0 Null

freeGui :: MonadIO m => GUI -> m ()
freeGui g = liftIO $
  mapM_ work $ g^.unGui._2.gstWTree
  where
    work (ctx,_) =
      freeCommonResource $ ctx^.ctxCmnRsc

-- modifyGui :: (Monad m, Functor m) => (GUI -> GUI) -> GuiT m ()
-- modifyGui f = do
--   GUI (_,stt) <- f . GUI <$> ((,) <$> ask <*> get)
--   put stt

getContextColorOfWidget :: (MonadReader GuiEnv m, MonadIO m) => Widget -> m ContextColor
getContextColorOfWidget w = do
  schemeMap <- asks geColorScheme
  liftIO $ case lookupColorOfWidget w schemeMap of
    Left err -> E.throwIO $ userError err
    Right a  -> return a

newCommonResource :: (RenderEnv m, MonadIO m) => V2 Int -> WidgetColor -> Widget -> m CommonResource
newCommonResource size wcol w =
  withRenderer $ \r ->
    CmnRsc <$> G.newFillRectangle r size'
           <*> G.newRectangle r size'
           <*> genTitle r wcol w
  where
    size' = fromIntegral <$> size

freeCommonResource :: CommonResource -> IO ()
freeCommonResource CmnRsc{..} = do
  G.freePrim cmnrscRectFill
  G.freePrim cmnrscRectBorder
  whenJust cmnrscTextTex G.deleteTexture

mkSingle :: (RenderEnv m, MonadIO m)
  => Maybe WTName -> Maybe ContextColor -> Style -> V2 UExp -> V2 UExp -> Widget -> GuiT m GuiWidgetTree
mkSingle mName mColor style pos size w = do
  ident <- WTIdent <$> use gstIdCnt
  gstIdCnt += 1
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  ctxCol <- maybe (getContextColorOfWidget w) return mColor
  cmnRsc <- lift $ newCommonResource (pure 1) (ctxcolNormal ctxCol) w
  let ctx = WContext ident mName Nothing (attribOf w) True True iniWidgetState cmnRsc ctxCol style pos' size'
  return $ Fork Null (ctx, w) Nothing Null

mkContainer :: (RenderEnv m, MonadIO m)
  => Maybe WTName -> ContainerType -> Maybe ContextColor -> Style -> V2 UExp -> V2 UExp -> GuiT m GuiWidgetTree
mkContainer mName ct mColor style pos size = do
  ident <- WTIdent <$> use gstIdCnt
  gstIdCnt += 1
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  let w = Transparent
  ctxCol <- maybe (getContextColorOfWidget w) return mColor
  cmnRsc <- lift $ newCommonResource (pure 1) (ctxcolNormal ctxCol) w
  let ctx = WContext ident mName (Just ct) (attribOf w) True True iniWidgetState cmnRsc ctxCol style pos' size'
  return $ Fork Null (ctx,w) (Just Null) Null

appendRoot :: Monad m => GuiWidgetTree -> GuiT m ()
appendRoot wt = modify $ over gstWTree (wt <>)

prependRoot :: Monad m => GuiWidgetTree -> GuiT m ()
prependRoot wt = modify $ over gstWTree (<> wt)

-- Rendering GUI

setAllNeedsLayout :: GUI -> GUI
setAllNeedsLayout =
  over (unGui._2.gstWTree) (fmap work)
  where
    work = set (_1 . ctxNeedsLayout) True

setAllNeedsRender :: GUI -> GUI
setAllNeedsRender =
  over (unGui._2.gstWTree) (fmap work)
  where
    work = set (_1 . ctxNeedsRender) True

-- | Ready for rendering. Call this at the end of Update
readyRender :: (RenderEnv m, MonadIO m) => GUI -> m (Bool, GUI)
readyRender g = do
  V2 w h <- getWindowSize
  let vmap = M.fromList [ (kKeyWidth, w), (kKeyHeight, h), (kKeyWinWidth, w), (kKeyWinHeight, h)]
  (updated, wt) <- go vmap $ g^.unGui._2.gstWTree
  let g' = g & unGui._2.gstWTree .~ (updateLayout . updateVisibility) wt
  return (updated, g')
  where
    go _ Null = return (False, Null)
    go vmap (Fork u a mc o) = do
      (updatedU, u') <- go vmap u
      needsRenderByItself <- case a of
                               (_,UserWidget c) -> liftIO $ needsRender c
                               _                -> return False
      (updatedA, a') <- if ctx^.ctxNeedsRender || needsRenderByItself
                        then (,) True <$> readyLayout (M.map fromIntegral vmap) a
                        else return (False, a)
      (updatedC, mc') <- case mc of
        Nothing -> return (False, Nothing)
        Just c -> do
          let (V2 w h) = fromIntegral <$> (a'^._1 . ctxWidgetState . wstSize)
              vmap' = M.insert kKeyWidth w . M.insert kKeyHeight h $ vmap -- Update width and height
          (updatedC, c') <- go vmap' c
          return (updatedC, Just c')
      (updatedO, o') <- go vmap o
      let updated = updatedU || updatedA || updatedC || updatedO
      return (updated, Fork u' a' mc' o')
      where
        ctx = a^._1

    readyLayout vmap (ctx, widget) = do
      pos <- P <$> evalExp2 upos
      size <- evalExp2 usize
      let size' = fromIntegral <$> size
      liftIO . freeCommonResource $ ctx^.ctxCmnRsc
      widget' <- onReadyLayout size' (optimumColor ctx) widget
      cmnrsc' <- newCommonResource size' (optimumColor ctx) widget'
      let ctx' = ctx & ctxNeedsRender .~ False
                     & ctxWidgetState . wstPos .~ pos
                     & ctxWidgetState . wstSize .~ size
                     & ctxCmnRsc .~ cmnrsc'
      return (ctx', widget')
      where
        upos = ctx^.ctxUPos
        usize = ctx^.ctxUSize

        evalExp2  (V2 x y) =
          case V2 <$> evalExp x <*> evalExp y of
            Left errmsg -> E.throw $ userError errmsg
            Right v2    -> return v2
          where
            evalExp (ERPN expr) = truncate <$> RPN.eval (vmap :: M.Map String Double) expr
            evalExp (EConst v)  = return v

render :: (RenderEnv m, MonadIO m) => GUI -> m ()
render g =
  withRenderer $ \r -> liftIO $
    mapM_ (go r) $ view (unGui._2.gstWTree) g
  where
    go r (ctx, widget)
      | ctx^.ctxNeedsRender            = E.throwIO $ userError "Call GUI.readyRender before GUI.render!"
      | ctx^.ctxWidgetState.wstVisible = renderWidget r pos size wcol style cmnrsc widget
      | otherwise                      = return ()
        where
          P pos = fromIntegral <$> ctx^.ctxWidgetState.wstGlobalPos
          size = fromIntegral <$> ctx^.ctxWidgetState^.wstSize
          wcol = optimumColor ctx
          style = ctx^.ctxStyle
          cmnrsc = ctx^.ctxCmnRsc
