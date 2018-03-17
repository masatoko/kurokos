{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Kurokos.UI.Core where

-- import Debug.Trace (traceM)
import Data.List.Extra (firstJust)
import qualified Data.Set as Set
import           Control.Concurrent.MVar
import qualified Control.Exception        as E
import           Control.Lens
import           Control.Monad.Extra      (whenJust)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString          (ByteString)
import qualified Data.Map                 as M
import qualified Data.Map.Strict          as MS
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
  (_,g2) <- readyRender g1 -- Generate textures
  (_,g3) <- readyRender $ setAllNeedsRender g2 -- Calculate position and size
  return $ g3 & unGui._2.gstWTree %~ WT.balance
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

-- readyRender :: (RenderEnv m, MonadIO m) => GUI -> m (Bool, GUI)
-- readyRender g = do
--   V2 w h <- getWindowSize
--   let vmap = M.fromList [ (kKeyWidth, w), (kKeyHeight, h), (kKeyWinWidth, w), (kKeyWinHeight, h)]
--   (updated, wt) <- go vmap $ g^.unGui._2.gstWTree
--   let g' = g & unGui._2.gstWTree .~ (updateLayout (V2 w h) . updateVisibility) wt
--   return (updated, g')
--   where
--     go _ Null = return (False, Null)
--     go vmap (Fork u a mc o) = do
--       (updatedU, u') <- go vmap u
--       needsRenderByItself <- case a of
--                                (_,UserWidget c) -> liftIO $ needsRender c
--                                _                -> return False
--       (updatedA, a') <- if ctx^.ctxNeedsRender || needsRenderByItself
--                         then (,) True <$> readyLayout (M.map fromIntegral vmap) a
--                         else return (False, a)
--       (updatedC, mc') <- case mc of
--         Nothing -> return (False, Nothing)
--         Just c -> do
--           let (V2 w h) = fromIntegral <$> (a'^._1 . ctxWidgetState . wstSize)
--               vmap' = M.insert kKeyWidth w . M.insert kKeyHeight h $ vmap -- Update width and height
--           (updatedC, c') <- go vmap' c
--           return (updatedC, Just c')
--       (updatedO, o') <- go vmap o
--       let updated = updatedU || updatedA || updatedC || updatedO
--       return (updated, Fork u' a' mc' o')
--       where
--         ctx = a^._1
--
--     readyLayout vmap (ctx, widget) = do
--       pos <- P <$> evalExp2 upos
--       size <- evalExp2 usize
--       let size' = fromIntegral <$> size
--       liftIO . freeCommonResource $ ctx^.ctxCmnRsc
--       widget' <- onReadyLayout size' (optimumColor ctx) widget
--       cmnrsc' <- newCommonResource size' (optimumColor ctx) widget'
--       let ctx' = ctx & ctxNeedsRender .~ False
--                      & ctxWidgetState . wstPos .~ pos
--                      & ctxWidgetState . wstSize .~ size
--                      & ctxCmnRsc .~ cmnrsc'
--       return (ctx', widget')
--       where
--         upos = ctx^.ctxUPos
--         usize = ctx^.ctxUSize
--
--         evalExp2  (V2 x y) =
--           case V2 <$> evalExp x <*> evalExp y of
--             Left errmsg -> E.throw $ userError errmsg
--             Right v2    -> return v2
--           where
--             evalExp (ERPN expr) = truncate <$> RPN.eval (vmap :: M.Map String Double) expr
--             evalExp (EConst v)  = return v

-- Update world position (wstWorldPos) in WidgetState
--
-- Update world position if needs layout and set ctxNeesLayout False
-- updateLayout :: GuiWidgetTree -> GuiWidgetTree
-- updateLayout wt0 = fst $ work wt0 Unordered False (P $ V2 0 0)
--   where
--     help f (a,p) = f p >> return a
--     modsize Unordered       _ = return ()
--     modsize VerticalStack   p = _y .= (p^._y)
--     modsize HorizontalStack p = _x .= (p^._x)
--
--     work Null            _   _            p0 = (Null, p0)
--     work (Fork u a mc o) ct0 parentLayout p0 = runState go p0
--       where
--         wst = a^._1.ctxWidgetState
--         shouldLayout = parentLayout || (a^._1.ctxNeedsLayout)
--         ct' = fromMaybe Unordered $ a^._1.ctxContainerType
--         go = do
--           -- Under
--           u' <- help (modsize ct0) . work u ct0 parentLayout =<< get
--           -- CtxWidget
--           pos <- get
--           let pos' = case ct0 of
--                       Unordered -> p0 + (wst^.wstPos)
--                       _         -> pos
--               a' = if shouldLayout
--                       then a & _1 . ctxWidgetState . wstWorldPos .~ pos'
--                              & _1 . ctxNeedsLayout .~ False
--                       else a
--           modsize ct0 $ pos' + P (wst^.wstSize)
--           -- Children
--           mc' <- case mc of
--             Nothing -> return Nothing
--             Just c  -> fmap Just $ help (modsize ct0) $ work c ct' shouldLayout pos'
--           -- Over
--           o' <- help (modsize ct0) . work o ct0 parentLayout =<< get
--           return $ Fork u' a' mc' o'

-- | Ready for rendering. Call this at the end of Update
readyRender :: (RenderEnv m, MonadIO m) => GUI -> m (Bool, GUI)
readyRender g = do
  winSize <- getWindowSize
  let wt0 = g^.unGui._2.gstWTree
      wt1 = updateLayout winSize . updateVisibility $ wt0
  (wt2, updated) <- runStateT (mapM ready wt1) False
  let g' = g&unGui._2.gstWTree .~ wt2
  return (updated, g')
  where
    ready a@(ctx,widget) = do
      needsRenderByItself <- case a of
                               (_,UserWidget c) -> liftIO $ needsRender c
                               _                -> return False
      let updatedA = ctx^.ctxNeedsRender || needsRenderByItself
      modify (updatedA &&)
      if updatedA
        then do
          liftIO . freeCommonResource $ ctx^.ctxCmnRsc
          widget' <- lift $ onReadyLayout size (optimumColor ctx) widget
          cmnrsc' <- lift $ newCommonResource size (optimumColor ctx) widget'
          let ctx' = ctx & ctxNeedsRender .~ False
                         & ctxCmnRsc .~ cmnrsc'
          return (ctx', widget)
        else return a
      where
        size = fromIntegral <$> ctx^.ctxWidgetState.wstSize

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

-- Update position (local and world) and size
updateLayout :: V2 CInt -> GuiWidgetTree -> GuiWidgetTree
updateLayout (V2 winW winH) wt0 = flip evalState minSizeMap0 $ do
  let vmap = M.insert kKeyWidth winW . M.insert kKeyHeight winH $ defVmap
  calcMinSize vmap wt0
  wt1 <- calcSize vmap wt0
  return $ snd $ calcWPos Unordered (pure 0) wt1
  where
    defVmap = M.fromList [(kKeyWinWidth, fromIntegral winW), (kKeyWinHeight, fromIntegral winH)]

    minSizeMap0 :: MS.Map WTIdent (V2 (Maybe CInt))
    minSizeMap0 = MS.empty

    getMinSize idx = MS.lookup idx <$> get
    writeW idx w = modify $ \pm ->
      case M.lookup idx pm of
        Nothing        -> M.insert idx (V2 (Just w) Nothing) pm
        Just (V2 _ mh) -> M.insert idx (V2 (Just w) mh) pm
    writeH idx h = modify $ \pm ->
      case M.lookup idx pm of
        Nothing        -> M.insert idx (V2 Nothing (Just h)) pm
        Just (V2 mw _) -> M.insert idx (V2 mw (Just h)) pm

    evalExp _    (EConst x) = Right x
    evalExp vmap (ERPN e)   = case RPN.eval (M.map fromIntegral vmap) e of
                                Left msg -> Left msg
                                Right w  -> Right $ floor w

    -- * Step.1
    --   - Calculate minimum size for Container (bottom-up)
    --   - Parent size is NOT calculated at this time, so kKeyWidth and kKeyHeight are not fixed.
    calcMinSize :: M.Map String CInt -> GuiWidgetTree -> State (M.Map WTIdent (V2 (Maybe CInt))) ([CInt], [CInt])
    calcMinSize _    Null                         = return ([],[])
    calcMinSize vmap (Fork u a@(ctx,widget) mc o) = do
      -- * Calc size (if decidable)
      let mw = case evalExp vmap expW of -- Use vmap from PARENT here
                Left _  -> Nothing -- Not decidable yet (kKeyMinWidth and kKeyMinHeight is not supplied)
                Right w -> Just w
          mh = case evalExp vmap expH of -- Use vmap from PARENT here
                Left _  -> Nothing -- Not decidable yet (..)
                Right h -> Just h
      -- traceM $ unwords [">>>", show (ctx^.ctxName), show mw, show mh, show vmap]
      -- * Minimum size
      case mc of
        Nothing -> do
          let (V2 mMinW mMinH) = widgetMinimumSize a
              w = fromMaybe 1 $ firstJust id [mw, mMinW] -- TODO: Reconsider
              h = fromMaybe 1 $ firstJust id [mh, mMinH]
          writeW idx w
          writeH idx h
        Just c  -> do -- Container
          let vmap4children = workH . workW $ defVmap
                where
                  workW = maybe id (M.insert kKeyWidth) mw
                  workH = maybe id (M.insert kKeyHeight) mh
          (ws,hs) <- calcMinSize vmap4children c
          -- traceM $ unwords [">>>+", show (ctx^.ctxName), show ws, show hs, show vmap4children, show (calcMinimumSize ws hs)]
          let V2 mMinW mMinH = calcMinimumSize ws hs
          whenJust mMinW $ writeW idx
          whenJust mMinH $ writeH idx

      -- * Calculate sizes for the parent
      (wsU, hsU) <- calcMinSize vmap u
      (wsO, hsO) <- calcMinSize vmap o
      let fw = maybe id (:) mw
          fh = maybe id (:) mh
      return (fw (wsU ++ wsO), fh (hsU ++ hsO)) -- Returns all sizes to the parent.
      where
        idx = ctx^.ctxIdent
        V2 expW expH = ctx^.ctxUSize
        calcMinimumSize ws hs =
          case thisCType of
            VerticalStack   -> V2 (help maximum ws) (help sum hs)
            HorizontalStack -> V2 (help sum ws) (help maximum hs)
            Unordered       -> V2 (help maximum ws) (help maximum hs) -- TODO: Considering position
          where
            thisCType = fromMaybe (error "No ContainerType for container") $ ctx^.ctxContainerType
            help _ [] = Nothing
            help f as = Just $ f as

    -- * Step.2
    --   - Calculate local pos and size using vmap with all keys (top-down)
    --   - Update ctxWidgetState.wstSize
    calcSize _     Null                    = return Null
    calcSize vmap0 (Fork u a@(ctx,_) mc o) = do
      -- * Complement vmap
      -- - Get minimum size
      mMinSize <- getMinSize idx
      let V2 mMinW mMinH = fromMaybe (error $ "Missing minimum size @" ++ show idx) mMinSize
          minW = fromMaybe 0 mMinW
          minH = fromMaybe 0 mMinH
      -- - Complement vmap
      let vmap = M.insert kKeyMinWidth minW . M.insert kKeyMinHeight minH $ vmap0
      -- * Decide size
      let w = case evalExp vmap expW of
                Left _  -> error $ "Can't eval width: " ++ show expW
                Right w -> w
          h = case evalExp vmap expH of
                Left _  -> error $ "Can't eval height: " ++ show expH
                Right h -> h
          x = case evalExp vmap expX of
                Left _  -> error $ "Can't eval pos.x: " ++ show expX
                Right x -> x
          y = case evalExp vmap expY of
                Left _  -> error $ "Can't eval pos.y: " ++ show expY
                Right y -> y
          a' = a&_1.ctxWidgetState.wstSize .~ V2 w h
                &_1.ctxWidgetState.wstPos  .~ P (V2 x y)
      -- traceM $ show (ctx^.ctxName) ++ " " ++ show (w,h) ++ " " ++ show vmap
      -- * Same depth
      u' <- calcSize vmap u
      o' <- calcSize vmap o
      -- * Children
      mc' <- case mc of
                Nothing -> return Nothing
                Just c  -> do
                  let vmap4cs = M.insert kKeyWidth w . M.insert kKeyHeight h $ vmap
                  Just <$> calcSize vmap4cs c
      return $ Fork u' a' mc' o'
      where
        idx = ctx^.ctxIdent
        V2 expW expH = ctx^.ctxUSize
        V2 expX expY = ctx^.ctxUPos

    -- * Step.3
    -- Calculate world position
    --   - Update ctxWidgetState.wstWorldPos
    --   - Top to down, under to over
    calcWPos :: ContainerType -> V2 CInt -> GuiWidgetTree -> (V2 CInt, GuiWidgetTree)
    calcWPos _     pos0 Null                    = (pos0, Null)
    calcWPos parCT pos0 (Fork u a@(ctx,_) mc o) =
      let (pos1, u') = calcWPos parCT pos0 u
          wpos = if parCT == Unordered
                  then P pos1 + (ctx^.ctxWidgetState.wstPos)
                  else P pos1
          a' = a&_1.ctxWidgetState.wstWorldPos .~ wpos
          pos2 = pos1 `advance` (ctx^.ctxWidgetState.wstSize)
          (pos3, mc') = case mc of
            Nothing -> (pos2, Nothing)
            Just c  ->
              case ctx^.ctxContainerType of
                Nothing    -> error "Missing ContainerType"
                Just ctype ->
                  let P wpos' = wpos
                  in Just <$> calcWPos ctype wpos' c
          (pos4, o') = calcWPos parCT pos3 o
      in (pos4, Fork u' a' mc' o')
      where
        advance pos0@(V2 x y) (V2 w h) = case parCT of
          Unordered       -> pos0
          VerticalStack   -> V2 x (y + h)
          HorizontalStack -> V2 (x + w) y


widgetMinimumSize :: (WContext, Widget) -> V2 (Maybe CInt)
widgetMinimumSize (ctx,w) = texSize
  where
    texSize = case cmnrscTextTex (ctx^.ctxCmnRsc) of
      Nothing  -> pure Nothing
      Just tex -> let V2 w h = (fromIntegral <$> G.texSize tex)
                  in V2 (Just w) (Just h)

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
          P pos = fromIntegral <$> ctx^.ctxWidgetState.wstWorldPos
          size = fromIntegral <$> ctx^.ctxWidgetState^.wstSize
          wcol = optimumColor ctx
          style = ctx^.ctxStyle
          cmnrsc = ctx^.ctxCmnRsc
