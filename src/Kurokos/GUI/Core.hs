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
import           Data.Maybe                (fromMaybe)
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
import           Kurokos.GUI.WidgetTree    (WidgetTree (..), wtappend)
import qualified Kurokos.GUI.WidgetTree    as WT
import qualified Kurokos.RPN               as RPN

data WContext = WContext
  { _ctxKey           :: WTKey
  , _ctxIdent         :: Maybe WidgetIdent
  , _ctxContainerType :: Maybe ContainerType
  , _ctxAttrib        :: WidgetAttrib
  , _ctxNeedsRender   :: Bool
  , _ctxWidgetState   :: WidgetState
  , _ctxColorSet      :: ColorSet
  , _ctxColor         :: WidgetColor
  , _ctxTexture       :: SDL.Texture
  , _ctxUPos          :: V2 Exp
  , _ctxUSize         :: V2 Exp
  }

makeLenses ''WContext

type GuiWidgetTree = WidgetTree (WContext, Widget)

-- map with parent position
mapWTPos :: (GuiPos -> (WContext, Widget) -> a) -> GuiWidgetTree -> WidgetTree a
mapWTPos f wt0 = fst $ work wt0 Unordered (pure 0)
  where
    modsize ct (x,p) = do
      case ct of
        Unordered      -> return ()
        VerticalStack   -> _y .= (p^._y)
        HorizontalStack -> _x .= (p^._x)
      return x

    work Null           _   p0 = (Null, p0)
    work (Single u a o) ct0 p0 = runState go p0
      where
        wst = a^._1.ctxWidgetState
        go = do
          u' <- modsize ct0 . work u ct0 =<< get
          pos <- get
          let pos' = case ct0 of
                      Unordered -> p0 + (wst^.wstPos)
                      _          -> pos
          let a' = f pos' a
          modsize ct0 ((), pos' + P (wst^.wstSize))
          o' <- modsize ct0 . work o ct0 =<< get
          return $ Single u' a' o'
    work (Container u a c o) ct0 p0 = runState go p0
      where
        wst = a^._1.ctxWidgetState
        ct' = fromMaybe Unordered $ a^._1.ctxContainerType
        go = do
          u' <- modsize ct0 . work u ct0 =<< get
          pos <- get
          let pos' = case ct0 of
                      Unordered -> p0 + (wst^.wstPos)
                      _          -> pos
          let a' = f pos' a
          modsize ct0 ((), pos' + P (wst^.wstSize))
          c' <- modsize ct0 $ work c ct' pos'
          o' <- modsize ct0 . work o ct0 =<< get
          return $ Container u' a' c' o'

mapWTPosM_ :: (Monad m, Applicative m) => (GuiPos -> (WContext, Widget) -> m a) -> GuiWidgetTree -> m ()
mapWTPosM_ f wt = void $ mapWTPosM f wt

mapWTPosM :: (Monad m, Applicative m) => (GuiPos -> (WContext, Widget) -> m a) -> GuiWidgetTree -> m (WidgetTree a)
mapWTPosM f wt0 = fst <$> work wt0 Unordered (pure 0)
  where
    modsize ct (x,p) = do
      case ct of
        Unordered      -> return ()
        VerticalStack   -> _y .= (p^._y)
        HorizontalStack -> _x .= (p^._x)
      return x

    work Null           _   p0 = return (Null, p0)
    work (Single u a o) ct0 p0 = runStateT go p0
      where
        wst = a^._1.ctxWidgetState
        go = do
          u' <- modsize ct0 =<< lift . work u ct0 =<< get
          pos <- get
          let pos' = case ct0 of
                      Unordered -> p0 + (wst^.wstPos)
                      _          -> pos
          a' <- lift $ f pos' a
          modsize ct0 ((), pos' + P (wst^.wstSize))
          o' <- modsize ct0 =<< lift . work o ct0 =<< get
          return $ Single u' a' o'
    work (Container u a c o) ct0 p0 = runStateT go p0
      where
        wst = a^._1.ctxWidgetState
        ct' = fromMaybe Unordered $ a^._1.ctxContainerType
        go = do
          u' <- modsize ct0 =<< lift . work u ct0 =<< get
          pos <- get
          let pos' = case ct0 of
                      Unordered -> p0 + (wst^.wstPos)
                      _          -> pos
          a' <- lift $ f pos' a
          modsize ct0 ((), pos' + P (wst^.wstSize))
          c' <- modsize ct0 =<< lift (work c ct' pos')
          o' <- modsize ct0 =<< lift . work o ct0 =<< get
          return $ Container u' a' c' o'

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
newGui env initializer = do
  gui <- runGuiT env iniGui initializer
  readyRender gui

genCtxS :: (RenderEnv m, MonadIO m, E.MonadThrow m)
  => Maybe WidgetIdent -> V2 UExp -> V2 UExp -> Widget -> GuiT m GuiWidgetTree
genCtxS mName pos size w = do
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
  let ctx = WContext key mName Nothing (attribOf w) True iniWidgetState colset (colorSetBasis colset) tex pos' size'
  return $ Single Null (ctx, w) Null

genContainer :: (RenderEnv m, MonadIO m, E.MonadThrow m)
  => ContainerType -> V2 UExp -> V2 UExp -> GuiT m GuiWidgetTree
genContainer ct pos size = do
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
      ctx = WContext key Nothing (Just ct) (attribOf w) True iniWidgetState colset (colorSetBasis colset) tex pos' size'
  return $ Container Null (ctx,w) Null Null

appendRoot :: Monad m => GuiWidgetTree -> GuiT m ()
appendRoot wt = modify $ over gWTree (wt `wtappend`)

prependRoot :: Monad m => GuiWidgetTree -> GuiT m ()
prependRoot wt = modify $ over gWTree (`wtappend` wt)

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
                     & ctxWidgetState . wstPos .~ pos
                     & ctxWidgetState . wstSize .~ size
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
      let (V2 w h) = fromIntegral <$> (a'^._1 . ctxWidgetState . wstSize)
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
render = mapWTPosM_ go . view gWTree
  where
    go pos0 (ctx,_)
      | ctx^.ctxNeedsRender = E.throw $ userError "Call GUI.readyRender before GUI.render!"
      | otherwise =
        renderTexture (ctx^.ctxTexture) $ Rectangle pos0 (ctx^.ctxWidgetState^.wstSize)
