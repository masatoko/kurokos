# kurokos-ui

## Usage

```haskell
import qualified Kurokos.UI              as UI

makeGui = do
  guiYaml <- liftIO $ B.readFile "gui.yaml"
  Asset.readAssetList "asset-list.yaml"
  ast <- Asset.loadAssetManager assetList
  r <- K.getRenderer
  (_,sdlAssets) <- allocate (Asset.newSDLAssetManager r ast)
                            Asset.freeSDLAssetManager
  styleMap <- liftIO $ UI.readStyleMap "style-scheme.yaml"
  gui <- alloc (UI.newGui (UI.GuiEnv sdlAssets styleMap) initGui) UI.freeGui
  return gui
  where
    initGui = do
      label = UI.newLabel "font-ident" "Label Text" fontSize
      wt <- UI.mkSingle (Just "name") ctxColor pos size
      UI.prependRoot wt
      where
        fontSize = 18
```

## Color Scheme

### Format

Yaml format

```yaml
{widget-name}: # `_default` or '_'+widget-name
  {context}:   # `default` or (normal|hover|click)
    back:   Red(0-255) Green(0-255) Blue(0-255) Alpha(0-255)
    border: Red Green Blue Alpha
    title:  Red Green Blue Alpha
    tint:   Red Green Blue Alpha
```

- widget-name
  - _default
  - _{[Names defined in 'src/Kurokos/UI/Widget/Names.hs'](src/Kurokos/UI/Widget/Names.hs)}


- context
  - default: Default colors. Adopt it if empty.
  - normal
  - hover
  - click

### Example

[../_data/gui-color-scheme.yaml](../_data/gui-color-scheme.yaml)

## User Definable Widget

```haskell
import           Control.Concurrent.MVar
import qualified Kurokos.UI              as UI

newtype UserVal = UserVal (MVar Int)

instance Renderable UserVal where
  -- | Render UserVal using renderer
  renderW renderer size (UserVal n) =
    -- Rendering UserVal code here

  -- | Rerender when it returns True
  -- It will be rerendered in `UI.readyRender`
  needsRender (UserVal mvar) = do
    n <- readMVar mvar
    return $ n `mod` 10 == 0

-- Making WidgetTree

mvar <- newMVar 0
let widget = UserWidget (UserVal mvar)
UI.newGui guiEnv $ do
  widgetTree <- UI.mkSingle Nothing Nothing pos size widget
  --
```

Or you can have Widget rerender by hand (Not using needsRender of Renderable).

```haskell
import Control.Lens

let gui' = UI.update (UI.isNameOf "target_name") (\cw -> cw&_1.ctxNeedsRender .~ True) gui
```
