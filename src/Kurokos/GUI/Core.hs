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
      { wtKey         :: WTKey
      , wtName        :: Maybe WidgetIdent
      , wtNeedsRender :: Bool
      , wtColorSet    :: ColorSet
      , wtColor       :: WidgetColor
      , wtTexture     :: SDL.Texture
      , wtTexInfo     :: TextureInfo
      , wtUPos        :: V2 Exp
      , wtUSize       :: V2 Exp
      , wtWidget      :: Widget
      }
  | Container
      { wtKey         :: WTKey
      , wtNeedsRender :: Bool
      , wtTexture     :: SDL.Texture
      , wtTexInfo     :: TextureInfo
      , wtUPos        :: V2 Exp
      , wtUSize       :: V2 Exp
      , wtChildren    :: [WidgetTree]
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
  { geFont            :: Font.Font
  , geDefaultColorSet :: ColorSet
  }

data GUI = GUI
  { _gKeyCnt         :: Key
  , _gWTrees         :: [WidgetTree]
  , _gEvents         :: [GuiEvent]
  , _gDragTrajectory :: [Point V2 Int32]
  } deriving Show

makeLenses ''GUI

iniGui :: GUI
iniGui = GUI 0 [] [] []

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
  readyRender gui

genSingle :: (RenderEnv m, MonadIO m, E.MonadThrow m)
  => Maybe WidgetIdent -> V2 UExp -> V2 UExp -> Widget -> GuiT m WidgetTree
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
  return $ Single key mName True colset (colorSetBasis colset) tex ti pos' size' w

genContainer :: (RenderEnv m, MonadIO m, E.MonadThrow m)
  => V2 UExp -> V2 UExp -> [WidgetTree] -> GuiT m WidgetTree
genContainer pos size ws = do
  key <- WTKey <$> use gKeyCnt
  gKeyCnt += 1
  pos' <- case fromUExpV2 pos of
            Left err -> E.throw $ userError err
            Right v  -> return v
  size' <- case fromUExpV2 size of
            Left err -> E.throw $ userError err
            Right v  -> return v
  let ti = TextureInfo (pure 0) (pure 1)
  tex <- lift $ withRenderer $ \r ->
    SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget (pure 1)
  return $ Container key True tex ti pos' size' ws

prependRoot :: Monad m => WidgetTree -> GuiT m ()
prependRoot w = modify $ over gWTrees (w:)

prependRootWs :: Monad m => [WidgetTree] -> GuiT m ()
prependRootWs ws = modify $ over gWTrees (ws ++)

-- Rendering GUI

setAllNeedsRender :: GUI -> GUI
setAllNeedsRender = over gWTrees (map go)
  where
    go wt@Single{..}    = wt {wtNeedsRender = True}
    go wt@Container{..} = wt {wtNeedsRender = True, wtChildren = map go wtChildren}

readyRender :: (RenderEnv m, MonadIO m, MonadMask m) => GUI -> m GUI
readyRender g = do
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
      let vmap' = M.map fromIntegral vmap
      -- liftIO $ do
      --   putStrLn "====="
      --   print vmap
      --   print vmap'
      --   print upos
      --   print usize
      pos <- case evalExp2 vmap' upos of
              Left err -> E.throw $ userError err
              Right v  -> return $ P v
      size <- case evalExp2 vmap' usize of
              Left err -> E.throw $ userError err
              Right v  -> return v
      -- liftIO . print $ (pos, size, fst <$> mWidget)
      tex <- case mWidget of
        Just (widget, wcol) -> createTexture' size wcol widget renderWidget
        Nothing             -> createDummyTexture size
      return (tex, TextureInfo pos size)

    go vmap wt@Single{..} =
      if wtNeedsRender
        then do
          SDL.destroyTexture wtTexture
          (tex, ti) <- makeTexture vmap wtUPos wtUSize (Just (wtWidget, wtColor))
          return $ wt {wtNeedsRender = False, wtTexture = tex, wtTexInfo = ti}
        else return wt
    go vmap wt@Container{} = do
      wt' <- if wtNeedsRender wt
              then do
                SDL.destroyTexture $ wtTexture wt
                (tex, ti) <- makeTexture vmap (wtUPos wt) (wtUSize wt) Nothing
                return $ wt {wtNeedsRender = False, wtTexture = tex, wtTexInfo = ti}
              else return wt
      let (V2 w h) = fromIntegral <$> (tiSize . wtTexInfo $ wt')
          vmap' = M.insert keyWidth w . M.insert keyHeight h $ vmap -- Update width and height
      ws <- mapM (go vmap') (wtChildren wt')
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
    go pos0 Single{..}
      | wtNeedsRender = E.throw $ userError "Call GUI.readyRender before GUI.render!"
      | otherwise = do
        let pos' = pos0 + tiPos
        renderTexture wtTexture $ Rectangle pos' tiSize
        where
          TextureInfo{..} = wtTexInfo
    go pos0 Container{..}
      | wtNeedsRender = E.throw $ userError "Call GUI.readyRender before GUI.render!"
      | otherwise = do
        let pos' = pos0 + tiPos
        mapM_ (go pos') wtChildren
        where
          TextureInfo{..} = wtTexInfo
