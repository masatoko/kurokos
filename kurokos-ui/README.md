# kurokos-ui

## Widget Name

[Defined in 'src/Kurokos/Widget/Names.hs'](src/Kurokos/Widget/Names.hs)

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
  - _{[Names defined in 'src/Kurokos/Widget/Names.hs'](src/Kurokos/Widget/Names.hs)}


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

data UserVal = UserVal (MVar Int)

instance Renderable UserVal where
  -- | Render UserVal using renderer
  renderW renderer size (UserVal n) = do
    -- Rendering UserVal code here

  -- | Rerender when it returns True
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
