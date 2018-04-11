{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Kurokos.UI.Core where

-- import           Debug.Trace              (trace, traceM)

import           Control.Concurrent.MVar
import qualified Control.Exception        as E
import           Control.Lens
import           Control.Monad.Extra      (whenJust)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString          (ByteString)
import           Data.Either              (lefts, rights)
import           Data.Foldable            (toList)
import           Data.List.Extra          (firstJust)
import qualified Data.Map                 as M
import qualified Data.Map.Strict          as MS
import           Data.Maybe               (fromMaybe, isJust)
import           Data.Monoid              ((<>))
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Linear.V2

import           SDL                      (($=))
import qualified SDL

import qualified Kurokos.Asset            as Asset
import qualified Kurokos.Asset.Raw        as Asset
import qualified Kurokos.RPN              as RPN

import qualified Kurokos.Graphics         as G
import           Kurokos.UI.Color
import           Kurokos.UI.Scheme        (StyleMap, makeContextStyle)
import           Kurokos.UI.Control       (GuiAction (..), GuiHandler (..),
                                           defaultGuiHandler)
import           Kurokos.UI.Event         (GuiEvent)
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import           Kurokos.UI.Widget.Render
import           Kurokos.UI.Widget.Update (onReadyLayout)
import           Kurokos.UI.WidgetTree    (WidgetTree (..))
import qualified Kurokos.UI.WidgetTree    as WT

type CtxWidget = (WContext, Widget)
type GuiWidgetTree = WidgetTree CtxWidget

data GuiEnv = GuiEnv
  { geAssetManager :: Asset.AssetManager
  , geStyleMap     :: StyleMap
  } deriving Show

data GuiState = GuiState
  { _gstIdCnt      :: WidgetIdent -- ^ Counter for WidgetTree ID
  , _gstWTree      :: GuiWidgetTree
  , _gstUpdated    :: Bool -- ^ Updated (should re-render)
  -- * Focus
  , _gstFocus      :: WT.WidgetTreePath
  -- * Control
  , _gstGuiHandler :: GuiHandler
  , _gstEvents     :: [GuiEvent]
  } deriving Show

makeLenses ''GuiState

newtype GUI = GUI { _unGui :: (GuiEnv, GuiState) } deriving Show

makeLenses ''GUI

getWidgetTree :: GUI -> GuiWidgetTree
getWidgetTree = view gstWTree . snd . _unGui

setWidgetTree :: GuiWidgetTree -> GUI -> GUI
setWidgetTree wt g = g&unGui._2.gstWTree .~ wt

modifyWidgetTree :: (GuiWidgetTree -> GuiWidgetTree) -> GUI -> GUI
modifyWidgetTree = over (unGui._2.gstWTree)

getGuiEvents :: GUI -> [GuiEvent]
getGuiEvents = view (unGui._2.gstEvents)

guiUpdated :: GUI -> Bool
guiUpdated = view (unGui._2.gstUpdated)

newtype GuiT m a = GuiT {
    runGT :: ReaderT GuiEnv (StateT GuiState m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader GuiEnv, MonadState GuiState)

runGuiT :: Monad m => GUI -> GuiT m a -> m (a, GUI)
runGuiT (GUI (env, gst)) k = do
  (a, gst') <- runStateT (runReaderT (runGT k) env) gst
  return (a, GUI (env, gst'))

instance MonadTrans GuiT where
  lift = GuiT . lift . lift

newGui :: (RenderEnv m, MonadIO m)
  => GuiEnv -> GuiT m a -> m (a, GUI)
newGui env initializer = do
  (a,g1) <- runGuiT g0 initializer
  let g2 = g1 & unGui._2.gstWTree %~ WT.balance
  g3 <- readyRender (setAllNeedsRender g2) -- Create textures
  return (a, g3)
  where
    g0 = GUI (env, gst0)
    gst0 = GuiState 0 Null False [] defaultGuiHandler []

freeGui :: MonadIO m => GUI -> m ()
freeGui g = freeGuiWidgetTree $ g^.unGui._2.gstWTree

freeGuiWidgetTree :: MonadIO m => GuiWidgetTree -> m ()
freeGuiWidgetTree = liftIO . mapM_ work
  where
    work (ctx,_) = freeCommonResource $ ctx^.ctxCmnRsc

-- modifyGui :: (Monad m, Functor m) => (GUI -> GUI) -> GuiT m ()
-- modifyGui f = do
--   GUI (_,stt) <- f . GUI <$> ((,) <$> ask <*> get)
--   put stt

getContextStyleOfWidget :: MonadReader GuiEnv m
                        => Widget
                        -> Maybe WTName
                        -> Maybe WTClass
                        -> Maybe StyleMap -- ^ Prior StyleMap
                        -> m ContextStyle
getContextStyleOfWidget w mName mCls mPriorStyleMap = do
  styleMap <- asks geStyleMap
  return $ makeContextStyle w mName mCls prior styleMap
  where
    prior = fromMaybe M.empty mPriorStyleMap

-- getContextColorOfWidget :: (MonadReader GuiEnv m, MonadIO m) => Widget -> m ContextColor
-- getContextColorOfWidget w = do
--   styleMap <- asks geStyleMap
--   liftIO $ case lookupColorOfWidget w styleMap of
--     Left err -> E.throwIO $ userError err
--     Right a  -> return a

newCommonResource :: (RenderEnv m, MonadIO m) => Asset.AssetManager -> V2 Int -> Style -> Widget -> m CommonResource
newCommonResource ast size style w =
  withRenderer $ \r ->
    CmnRsc <$> G.newFillRectangle r size'
           <*> G.newRectangle r size'
           <*> genTitle ast r style w
  where
    size' = fromIntegral <$> size

freeCommonResource :: CommonResource -> IO ()
freeCommonResource CmnRsc{..} = do
  G.freePrim cmnrscRectFill
  G.freePrim cmnrscRectBorder
  whenJust cmnrscTextTex G.deleteTexture

clickableSize :: CtxWidget -> V2 CInt
clickableSize (ctx,w) =
  fromMaybe size $ additionalClickableSize ctx w
  where
    size = wstSize $ ctx^.ctxWidgetState

mkSingle :: (RenderEnv m, MonadIO m)
  => WidgetConfig -> Widget -> GuiT m GuiWidgetTree
mkSingle conf widget = do
  ident <- WTIdent <$> use gstIdCnt
  gstIdCnt += 1
  pos' <- case fromUExpV2 (V2 (wconfPosX conf) (wconfPosY conf)) of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 (V2 (wconfWidth conf) (wconfHeight conf)) of
            Left err -> E.throw $ userError err
            Right v  -> return v
  ctxst <- getContextStyleOfWidget widget (wconfName conf) (wconfClass conf) (wconfStyleMap conf)
  ast <- asks geAssetManager
  cmnRsc <- lift $ newCommonResource ast (pure 1) (ctxstNormal ctxst) widget
  let attrib = fromMaybe (attribOf widget) $ wconfAttrib conf
      ctx = WContext ident (wconfName conf) (wconfClass conf) [] Nothing attrib True True iniWidgetState cmnRsc ctxst pos' size'
  return $ Fork Null (ctx, widget) Nothing Null

mkContainer :: (RenderEnv m, MonadIO m)
  => WidgetConfig -> ContainerType -> GuiT m GuiWidgetTree
mkContainer conf ct = do
  ident <- WTIdent <$> use gstIdCnt
  gstIdCnt += 1
  pos' <- case fromUExpV2 (V2 (wconfPosX conf) (wconfPosY conf)) of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 (V2 (wconfWidth conf) (wconfHeight conf)) of
            Left err -> E.throw $ userError err
            Right v  -> return v
  ctxst <- getContextStyleOfWidget widget (wconfName conf) (wconfClass conf) (wconfStyleMap conf)
  ast <- asks geAssetManager
  cmnRsc <- lift $ newCommonResource ast (pure 1) (ctxstNormal ctxst) widget
  let attrib = fromMaybe attribCntn $ wconfAttrib conf
      ctx = WContext ident (wconfName conf) (wconfClass conf) [] (Just ct) attrib True True iniWidgetState cmnRsc ctxst pos' size'
  return $ Fork Null (ctx, widget) (Just Null) Null
  where
    widget = Fill
    attribCntn = WidgetAttrib
      { _hoverable = False
      , _clickable = False
      , _draggable = False
      , _droppable = False
      , _visible   = True
      , _scrollable = True
      }

appendRoot :: Monad m => GuiWidgetTree -> GuiT m ()
appendRoot wt = modify $ over gstWTree (wt <>)

prependRoot :: Monad m => GuiWidgetTree -> GuiT m ()
prependRoot wt = modify $ over gstWTree (<> wt)

modifyRoot :: MonadIO m => (GuiWidgetTree -> m GuiWidgetTree) -> GuiT m ()
modifyRoot f = do
  wt <- use gstWTree
  wt' <- lift $ f wt
  gstWTree .= wt'

-- Rendering GUI

setAllNeedsResize :: GUI -> GUI
setAllNeedsResize =
  over (unGui._2.gstWTree) (fmap setNeedsResize)

setNeedsResize :: CtxWidget -> CtxWidget
setNeedsResize = set (_1.ctxNeedsResize) True

setAllNeedsRender :: GUI -> GUI
setAllNeedsRender =
  over (unGui._2.gstWTree) (fmap work)
  where
    work cw = setNeedsResize $ cw&_1.ctxNeedsRender .~ True

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
--                      & ctxWidgetState . wstLocalPos .~ pos
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
--                       Unordered -> p0 + (wst^.wstLocalPos)
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
readyRender :: (RenderEnv m, MonadIO m) => GUI -> m GUI
readyRender g = do
  let g0 = g & unGui._2.gstWTree %~ updatePath . updateVisibility
  g1 <- work True g0 -- * Make Textures.
  work False g1 -- * Resize with texture size.
  where
    assetManager = geAssetManager $ g^.unGui._1
    work isFirstPath g = do
      winSize <- getWindowSize
      let wt0 = g^.unGui._2.gstWTree
          wt1 = updateLayout winSize wt0
      (wt2, updated) <- runStateT (mapM (ready isFirstPath) wt1) False
      return $ g&unGui._2.gstWTree .~ wt2
                &unGui._2.gstUpdated ||~ updated

    ready isFirstPath a@(ctx,widget) = do
      needsRenderByItself <- case a of
                               (_,UserWidget c) -> liftIO $ needsRender c
                               _                -> return False
      let updatedA = ctx^.ctxNeedsRender || needsRenderByItself
      modify (updatedA ||)
      if updatedA
        then do
          liftIO . freeCommonResource $ ctx^.ctxCmnRsc
          widget' <- lift $ onReadyLayout assetManager size (optimumStyle ctx) widget
          cmnrsc' <- lift $ newCommonResource assetManager size (optimumStyle ctx) widget'
          let ctx' = ctx & ctxNeedsRender .~ isFirstPath -- Should re-render on second path. Because min-width and min-height will change with Texture size.
                         & ctxNeedsResize .~ isFirstPath -- Same as above
                         & ctxCmnRsc .~ cmnrsc'
          return (ctx', widget')
        else return a
      where
        size = fromIntegral <$> wstSize (ctx^.ctxWidgetState)

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

data SizeDependency
  = SDIndependent
  | SDParent
  | SDChild
  deriving (Eq, Show)

updateLayout :: V2 CInt -> GuiWidgetTree -> GuiWidgetTree
updateLayout size wt0 =
  updateLayout_ size $ fmap (over _1 clearSize) wt0
  where
    clearSize ctx
      | ctx^.ctxNeedsResize = ctx & ctxNeedsResize .~ False
                                  & ctxWidgetState.wstWidth .~ Nothing
                                  & ctxWidgetState.wstHeight .~ Nothing
      | otherwise = ctx

-- Update position (local and world) and size
updateLayout_ :: V2 CInt -> GuiWidgetTree -> GuiWidgetTree
updateLayout_ (V2 winW winH) wt0
  | all isLayouted wt0 = wt0
  | otherwise          =
    let vmap = M.insert kKeyWidth winW . M.insert kKeyHeight winH $ defVmap
        wt' = flip evalState minSizeMap0 $ do
                let calcTillLayouted i notLayoutedList wt = do
                      calcMinSize i vmap wt
                      wt' <- calcSize i vmap wt
                      -- traceCurrentState i wt'
                      let notLayoutedList' = getNotLayouted wt'
                          notChanged = notLayoutedList' == notLayoutedList
                      if | all isLayouted wt' -> return wt'
                         | notChanged         -> error $ "Cannot resolve layout: " ++ show notLayoutedList ++ "\n" ++ WT.prettyWith (show . snd) wt'
                         | otherwise          -> calcTillLayouted (i+1) notLayoutedList' wt'
                wt1 <- calcTillLayouted 0 (getNotLayouted wt0) wt0
                wt2 <- setMinSize wt1
                calcLocalPos vmap wt2
    in snd $ calcWPos Unordered (pure 0) wt'
  where
    defVmap = M.fromList [(kKeyWinWidth, fromIntegral winW), (kKeyWinHeight, fromIntegral winH)]

    getNotLayouted = map (view (_1.ctxIdent)) . filter (not . isLayouted) . toList

    -- * for debug
    -- traceCurrentState i wt = do
    --   m <- get
    --   traceM $ "- TRY" ++ show i ++ " " ++ show m
    --   mapM_ work wt
    --   where
    --     work (ctx,_) = traceM $ unwords ["TRY", show i, ": #", show (unWidgetIdent $ ctx^.ctxIdent), show (wst^.wstWidth), show (wst^.wstHeight)]
    --       where
    --         wst = ctx^.ctxWidgetState

    isLayouted :: CtxWidget -> Bool
    isLayouted (ctx,_) = isJust (wst^.wstWidth) && isJust (wst^.wstHeight)
      where
        wst = ctx^.ctxWidgetState

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
    calcMinSize :: Int -> M.Map String CInt -> GuiWidgetTree -> State (M.Map WTIdent (V2 (Maybe CInt))) ([Either SizeDependency CInt], [Either SizeDependency CInt])
    calcMinSize _ _    Null                         = return ([],[])
    calcMinSize i vmap (Fork u a@(ctx,widget) mc o) = do
      -- * Calc size (if decidable)
      let mw = firstJust id [ctx^.ctxWidgetState.wstWidth, eitherToMaybe (evalExp vmap expW)]
          mh = firstJust id [ctx^.ctxWidgetState.wstHeight, eitherToMaybe (evalExp vmap expH)]
      -- * Minimum size
      case mc of
        Nothing -> do
          let (V2 mMinW mMinH) = widgetMinimumSize a
              mMinW' = firstJust id [mMinW, mw] -- TODO: Reconsider
              mMinH' = firstJust id [mMinH, mh]
              mgn = style^.styleMargin
                where
                  style = optimumStyle ctx
          whenJust mMinW' $ writeW idx
          whenJust mMinH' $ writeH idx
        Just Null -> do -- Container has no children
          -- Set dummy minimum size when children is empty.
          writeW idx 1
          writeH idx 1
        Just c -> do -- Container
          let vmap4children = workH . workW $ defVmap
                where
                  workW = maybe id (M.insert kKeyWidth) mw
                  workH = maybe id (M.insert kKeyHeight) mh
          (ws,hs) <- calcMinSize i vmap4children c
          -- * Not decide minimum size if depends children's size
          let ws' = if SDChild `elem` lefts ws then [] else rights ws
              hs' = if SDChild `elem` lefts hs then [] else rights hs
          let V2 mMinW mMinH = calcMinimumSize ws' hs'
          -- traceM $ unwords ["!", show i, show (ctx^.ctxIdent), show (ctx^.ctxName), show mMinH, show vmap4children]
          whenJust mMinW $ writeW idx
          whenJust mMinH $ writeH idx

      -- * Calculate sizes for the parent
      (wsU, hsU) <- calcMinSize i vmap u
      (wsO, hsO) <- calcMinSize i vmap o
      let ew = maybe (Left sdw) (Right . (+ marginW)) mw
          eh = maybe (Left sdh) (Right . (+ marginH)) mh
      return (ew:wsU ++ wsO, eh:hsU ++ hsO) -- Returns all sizes to the parent.
      where
        idx = ctx^.ctxIdent

        margin = style^.styleMargin
          where
            style = optimumStyle ctx
        marginW = fromIntegral $ left margin + right margin
        marginH = fromIntegral $ top margin + bottom margin

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

        sdw = sizeDependency expW
        sdh = sizeDependency expH
        sizeDependency EConst{} = SDIndependent
        sizeDependency (ERPN e)
          | kKeyWidth `elem` cs    || kKeyHeight `elem` cs    = SDParent
          | kKeyMinWidth `elem` cs || kKeyMinHeight `elem` cs = SDChild
          | otherwise                                         = SDIndependent
          where
            cs = RPN.consts e

    -- | Complement vmap
    complementMinSize idx vmap = do
      mMinSize <- getMinSize idx -- Get minimum size
      let V2 mMinW mMinH = fromMaybe (V2 Nothing Nothing) mMinSize
      -- - Complement vmap
      let workW = maybe id (M.insert kKeyMinWidth) mMinW
          workH = maybe id (M.insert kKeyMinHeight) mMinH
      return $ workW . workH $ vmap

    -- * Step.2
    --   - Calculate local pos and size using vmap with all keys (top-down)
    --   - Update ctxWidgetState.wstSize
    calcSize _ _     Null                    = return Null
    calcSize i vmap0 (Fork u a@(ctx,_) mc o) = do
      vmap <- complementMinSize idx vmap0
      -- * Decide size
      let mw = case evalExp vmap expW of
                Left _  -> Nothing
                Right w -> Just w
          mh = case evalExp vmap expH of
                Left _  -> Nothing
                Right h -> Just h
          a' = a&_1.ctxWidgetState.wstWidth  .~ (fromIntegral <$> mw)
                &_1.ctxWidgetState.wstHeight .~ (fromIntegral <$> mh)
      -- traceM $ unwords [show i, show (ctx^.ctxIdent), show (ctx^.ctxName), show mh, show vmap0, show vmap]
      -- * Same depth
      u' <- calcSize i vmap0 u
      o' <- calcSize i vmap0 o
      -- * Children
      mc' <- case mc of
                Nothing -> return Nothing
                Just c  -> do
                  let vmap4cs = workW . workH $ defVmap
                        where
                          workW = maybe id (M.insert kKeyWidth) mw
                          workH = maybe id (M.insert kKeyHeight) mh
                  Just <$> calcSize i vmap4cs c
      return $ Fork u' a' mc' o'
      where
        idx = ctx^.ctxIdent
        V2 expW expH = ctx^.ctxUSize
        V2 expX expY = ctx^.ctxUPos

    -- | Set wstMinSize
    setMinSize = mapM work
      where
        work (ctx,w) = do
          mMinSize <- getMinSize idx -- Get minimum size
          let minSize = fromMaybe (pure Nothing) mMinSize
              ctx' = ctx&ctxWidgetState.wstMinSize .~ minSize
          return (ctx',w)
          where
            idx = ctx^.ctxIdent

    -- * Step.3
    calcLocalPos _     Null            = return Null
    calcLocalPos vmap0 (Fork u a@(ctx,_) mc o) = do
      u' <- calcLocalPos vmap0 u
      o' <- calcLocalPos vmap0 o
      -- * Complement vmap
      vmap <- complementMinSize idx vmap0
      let x = case evalExp vmap expX of
                Left _  -> error $ "Can't eval pos.x: " ++ show expX ++ " " ++ show vmap
                Right x -> x
          y = case evalExp vmap expY of
                Left _  -> error $ "Can't eval pos.y: " ++ show expY ++ " " ++ show vmap
                Right y -> y
          a' = a&_1.ctxWidgetState.wstLocalPos .~ P (V2 x y)
      mc' <- case mc of
              Nothing -> return Nothing
              Just c -> do
                let vmap4cs = workW . workH $ defVmap
                      where
                        workW = M.insert kKeyWidth w
                        workH = M.insert kKeyHeight h
                Just <$> calcLocalPos vmap4cs c
      return $ Fork u' a' mc' o'
      where
        idx = ctx^.ctxIdent
        V2 expW expH = ctx^.ctxUSize
        V2 expX expY = ctx^.ctxUPos
        V2 w h = wstSize $ ctx^.ctxWidgetState

    -- * Step.4
    -- Calculate world position
    --   - Update ctxWidgetState.wstWorldPos
    --   - Top to down, under to over
    calcWPos :: ContainerType -> V2 CInt -> GuiWidgetTree -> (V2 CInt, GuiWidgetTree)
    calcWPos _     pos0 Null                    = (pos0, Null)
    calcWPos parCT pos0 (Fork u a@(ctx,_) mc o) =
      let (pos1, u') = calcWPos parCT pos0 u
          wpos = case parCT of
                  Unordered       -> P pos1 + localPos
                  HorizontalStack -> P (pos1 + marginLT) & _y +~ dy
                  VerticalStack   -> P (pos1 + marginLT) & _x +~ dx
            where
              localPos@(P (V2 dx dy)) = ctx^.ctxWidgetState.wstLocalPos
          a' = a&_1.ctxWidgetState.wstWorldPos .~ wpos
          pos2 = pos1 `advance` size'
            where
              size' = size + marginSize
              size = fromIntegral <$> wstSize (ctx^.ctxWidgetState)
          (_, mc') = case mc of
            Nothing -> (pos2, Nothing)
            Just c  ->
              case ctx^.ctxContainerType of
                Nothing    -> error "Missing ContainerType"
                Just ctype -> do
                  let P wpos' = wpos
                      wpos'' = wpos' + (ctx^.ctxWidgetState.wstShift) -- Shift for children
                  Just <$> calcWPos ctype wpos'' c
          (pos3, o') = calcWPos parCT pos2 o
      in (pos3, Fork u' a' mc' o')
      -- in trace (unwords [show (ctx^.ctxIdent), show (wstSize (ctx^.ctxWidgetState)), " : ", show pos0, show pos1, show pos2, show pos3]) $ (pos3, Fork u' a' mc' o')
      where
        margin = style^.styleMargin
          where
            style = optimumStyle ctx
        marginLT = fromIntegral <$> V2 (left margin) (top margin)
        marginW = fromIntegral $ left margin + right margin
        marginH = fromIntegral $ top margin + bottom margin
        marginSize = V2 marginW marginH

        advance p@(V2 x y) (V2 w h) = case parCT of
          Unordered       -> p
          VerticalStack   -> V2 x (y + h)
          HorizontalStack -> V2 (x + w) y

widgetMinimumSize :: (WContext, Widget) -> V2 (Maybe CInt)
widgetMinimumSize (ctx,widget) =
  let w = firstJust id [widthFromCtx, widthFromWidget widget]
      h = firstJust id [heightFromCtx, heightFromWidget widget]
  in V2 w h
  where
    style = optimumStyle ctx
    fontSize = style^.styleFontSize

    widthFromCtx = do
      tex <- cmnrscTextTex $ ctx^.ctxCmnRsc
      let V2 w _ = G.texSize tex
      return $ fromIntegral w
    heightFromCtx = do
      tex <- cmnrscTextTex $ ctx^.ctxCmnRsc
      let V2 _ h = G.texSize tex
      return $ fromIntegral h

    widthFromWidget (TextField _ mRsc) =
      case mRsc of
        Nothing -> Just 10
        Just r  ->
          let widthL = maybe 0 (view _x . G.texSize) $ txtFldRscLeft r
              widthR = maybe 0 (view _x . G.texSize) $ txtFldRscRight r
          in Just . fromIntegral $ widthL + widthR + 10
    widthFromWidget (Picker _ _ ts) =
      Just $ case ts of
              [] -> 1
              _  -> fromIntegral . maximum $ map (view _x . G.texSize) ts
    widthFromWidget _ = Nothing

    heightFromWidget TextField{} = Just $ fromIntegral fontSize
    heightFromWidget Picker{}    = Just $ fromIntegral fontSize
    heightFromWidget _           = Nothing

updatePath :: GuiWidgetTree -> GuiWidgetTree
updatePath = fmap work . WT.wtPath
  where
    work ((ctx, w), path) =
      let ctx' = ctx&ctxPath .~ path
      in (ctx', w)

-- | Render GUI if GUI is updated
renderWhenUpdated :: (RenderEnv m, MonadIO m) => GUI -> m ()
renderWhenUpdated g = when (g^.unGui._2.gstUpdated) $ render g

type RenderArea = (V2 Int, V2 Int) -- ^ (position, size)

intersectRA :: RenderArea -> RenderArea -> Maybe RenderArea
intersectRA (V2 x0 y0, V2 w0 h0) (V2 x1 y1, V2 w1 h1) = do
  (x,w) <- work x0 w0 x1 w1
  (y,h) <- work y0 h0 y1 h1
  return (V2 x y, V2 w h)
  where
    work a0 da b0 db
      | m < n     = Just (m, n - m)
      | otherwise = Nothing
      where
        a1 = a0 + da
        b1 = b0 + db
        m = max a0 b0
        n = min a1 b1

-- | Render GUI
render :: (RenderEnv m, MonadIO m) => GUI -> m ()
render g = do
  winSize <- fmap fromIntegral <$> getWindowSize
  withRenderer $ \r -> liftIO $ do
    render_ r winSize True $ view (unGui._2.gstWTree) g
    render_ r winSize False $ view (unGui._2.gstWTree) g
    G.clearRenderArea r
  where
    render_ r winSize pFirst = go (pure 0, fromIntegral <$> winSize)
      where
        go _    Null            = return ()
        go area (Fork u a mc o) = do
          go area u
          work area winSize r pFirst a
          whenJust mc $ \c ->
            whenJust (area `intersectRA` posSizeOf (fst a)) $ \area' ->
              go area' c
          go area o

    posSizeOf ctx = (pos, size)
      where
        P pos = fromIntegral <$> ctx^.ctxWidgetState.wstWorldPos
        size = fromIntegral <$> wstSize (ctx^.ctxWidgetState)

    work (areaPos, areaSize) winSize r pFirst (ctx, widget)
      | ctx^.ctxNeedsRender            = E.throwIO $ userError "Call GUI.readyRender before GUI.render!"
      | ctx^.ctxWidgetState.wstVisible = do
          let render' = renderWidget r focus pos size ctx style cmnrsc widget
          if focus && topWhenFocused widget
            then
              unless pFirst $ do
                G.clearRenderArea r
                render'
            else when pFirst $ do
              G.setRenderArea r winSize areaPos areaSize
              render'
      | otherwise                      = return ()
        where
          focus = (g^.unGui._2.gstFocus) == ctx^.ctxPath
          (pos, size) = posSizeOf ctx
          style = optimumStyle ctx
          cmnrsc = ctx^.ctxCmnRsc

          topWhenFocused :: Widget -> Bool
          topWhenFocused Picker{} = True
          topWhenFocused _        = False

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
