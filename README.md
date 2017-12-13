# kurokos

WIP

## 注意！
Kurokosは開発中であり、APIは頻繁に変更されます!

まだ実際のゲーム開発に利用するべきではありません。

## Dependencies

![Dependencies](./doc/depend.png)

## Features
- Frame management
- Scene management
- GUI (Graphical User Interface)

# Install SDL2

## Windows

### Install Msys2
[http://www.msys2.org/](http://www.msys2.org/)

### Install libraries
```sh
pacman -S mingw64/mingw-w64-x86_64-SDL2
pacman -S mingw64/mingw-w64-x86_64-SDL2_gfx
pacman -S mingw64/mingw-w64-x86_64-SDL2_image
# pacman -S mingw64/mingw-w64-x86_64-SDL2_mixer
pacman -S mingw64/mingw-w64-x86_64-SDL2_ttf
```

### Environment

最新バージョンでは実行時にエラーが発生（17/11/26）

以下のバージョンで動作検証している。

| Library    | Version  |
|:-----------|:---------|
| SDL2       | 2.0.4-1  |
| SDL2_gfx   | 1.0.1-2  |
| SDL2_image | 2.0.1-1  |
| SDL2_mixer | 2.0.1-1  |
| SDL2_ttf   | 2.0.14-1 |

## Ubuntu

```sh
sudo apt-get install libsdl2-dev
sudo apt-get install libsdl2-image-dev
sudo apt-get install libsdl2-ttf-dev
# sudo apt-get install libsdl2-mixer-dev
sudo apt-get install libsdl2-gfx-dev
```

# Demo

## 1. フォントファイルを用意する

https://ipafont.ipa.go.jp/node26#jp の「IPAフォントライセンス」に同意をしてから、以下の方法でフォントを配置します。

### 自動

```
sh setup-font.sh
```

### マニュアル

https://oscdl.ipa.go.jp/IPAexfont/ipaexg00301.zip
をダウンロードし、`_data/font/`内に解凍した全てのTTFファイルを配置する。

## 2. コンパイルと実行

```sh
stack build --flag=kurokos:demo
stack exec demo
```

# Memo

## Make GUI using Asset

![make-gui-using-asset](./doc/make-gui-using-asset.png)

# TODOs

### GUI
- [x] Stack Container
- [ ] Scroll Container
- [x] Load GUI from file
- [x] Image View
- [ ] Text Area
- [ ] Text Field (Editable)
- [ ] Segmented Control (Radio Button)
- [ ] Select by cursor buttons (using zipper?)

### Others
- [x] File Archiver
- [ ] Localization Manager
- [ ] Asset Manager
- [ ] Effect System (ex. Particle)
