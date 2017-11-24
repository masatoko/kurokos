#include "ttf_helper.h"

extern DECLSPEC int SDLCALL
  TTF_OpenFontRW_(SDL_RWops *src, int ptsize)
{
  return TTF_OpenFontRW(src, 1, ptsize);
}
