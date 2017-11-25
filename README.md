# kurokos

WIP

- Frame management
- Scene management

# Install SDL2

## Windows

Install [msys2](http://www.msys2.org/)

```sh
pacman -S mingw64/mingw-w64-x86_64-SDL2
pacman -S mingw64/mingw-w64-x86_64-SDL2_gfx
pacman -S mingw64/mingw-w64-x86_64-SDL2_image
pacman -S mingw64/mingw-w64-x86_64-SDL2_mixer
pacman -S mingw64/mingw-w64-x86_64-SDL2_ttf
```

Environment

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
sudo apt-get install libsdl2-mixer-dev
sudo apt-get install libsdl2-gfx-dev
```
