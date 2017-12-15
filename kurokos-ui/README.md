# kurokos-ui

##

## Widget Name

[Defined in 'src/Kurokos/Widget/Names.hs'](src/Kurokos/Widget/Names.hs)

## Color Scheme

### Format

```yaml
{widget-name}:
  {context}:
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
