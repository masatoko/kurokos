#include "SDL2/SDL.h"
#include "SDL2/SDL_ttf.h"

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUNICODE_Blended_(TTF_Font *font, uint16_t *text, SDL_Color *fg)
{
  return TTF_RenderUNICODE_Blended(font, text, *fg);
}

// int TTF_GlyphMetrics(TTF_Font *font, Uint16 ch, int *minx, int *maxx, int *miny, int *maxy, int *advance)

typedef struct {
  int minx;
  int maxx;
  int miny;
  int maxy;
  int advance;
} CGlyphMetrics;

extern DECLSPEC int SDLCALL
  minx_TTF_GlyphMetrics(TTF_Font *font, int ch, CGlyphMetrics* gm)
{
  Uint16 ch16 = (Uint16) ch;
  int minx, maxx, miny, maxy, advance;
  const int result = TTF_GlyphMetrics(font, ch16, &minx, &maxx, &miny, &maxy, &advance);
  gm->minx = minx;
  gm->maxx = maxx;
  gm->miny = miny;
  gm->maxy = maxy;
  gm->advance = advance;
  return result;
}
