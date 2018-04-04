{-# LANGUAGE TemplateHaskell #-}
module Kurokos.UI.WidgetTree where

import           Control.Lens
import           Data.Foldable   (toList)
import           Data.List.Extra (firstJust)
import           Data.Maybe      (catMaybes)
import           Data.Monoid     ((<>))

data WidgetTree a
  = Null
  | Fork
    { _wtUnder    :: WidgetTree a
    , _wtElement  :: a
    , _wtChildren :: Maybe (WidgetTree a)
    , _wtOver     :: WidgetTree a
    }
  deriving (Eq, Show, Read)

makeLenses ''WidgetTree

mkNull :: WidgetTree a
mkNull = Null

showTree :: Show a => WidgetTree a -> String
showTree = unlines . work 0
  where
    indent n = replicate (4 * n) ' '

    work _ Null = []
    work n (Fork u a Nothing o) =
      work (n + 1) u
        ++ [indent n ++ "+ " ++ show a]
        ++ work (n + 1) o
    work n (Fork u a (Just c) o) =
      work (n + 1) u
        ++ [indent n ++ "@ " ++ show a]
        ++ work (n + 1) c
        ++ work (n + 1) o

pretty :: Show a => WidgetTree a -> String
pretty = unlines . work 0
  where
    indent n = replicate (2 * n) ' '

    work _ Null = []
    work n (Fork u a Nothing o) =
      work n u ++ [indent n ++ "+ " ++ show a] ++ work n o
    work n (Fork u _ (Just c) o) =
      work n u ++ [indent n ++ "@ | Container"] ++ work (n+1) c ++ work n o

prettyWith :: (a -> String) -> WidgetTree a -> String
prettyWith f = unlines . work 0
  where
    indent n = replicate (2 * n) ' '

    work _ Null = []
    work n (Fork u a Nothing o) =
      work n u ++ [indent n ++ "+ " ++ f a] ++ work n o
    work n (Fork u a (Just c) o) =
      work n u ++ [indent n ++ "@ | Container: " ++ f a] ++ work (n+1) c ++ work n o

size :: WidgetTree a -> Int
size Null            = 0
size (Fork u _ mc o) = size u + 1 + maybe 0 size mc + size o

instance Monoid (WidgetTree a) where
  mempty = Null

  mappend k Null            = k
  mappend Null k            = k
  mappend k (Fork u a mc o) = Fork (k `mappend` u) a mc o

prepend :: a -> WidgetTree a -> WidgetTree a
prepend k Null            = Fork Null k Nothing Null
prepend k (Fork u a mc o) = Fork (prepend k u) a mc o

append :: a -> WidgetTree a -> WidgetTree a
append k Null            = Fork Null k Nothing Null
append k (Fork u a mc o) = Fork u a mc (append k o)

wtFromList :: [a] -> WidgetTree a
wtFromList [] = Null
wtFromList as = Fork (wtFromList us) a Nothing (wtFromList os)
  where
    k = length as `div` 2
    (us, a:os) = splitAt k as

-- TODO: Add phantom type
appendChild :: WidgetTree a -> WidgetTree a -> WidgetTree a
appendChild (Fork u a (Just c) o) wt = Fork u a (Just $ c <> wt) o
appendChild _ _                      = error "appendChild"

-- TODO: Add phantom type
prependChild :: WidgetTree a -> WidgetTree a -> WidgetTree a
prependChild wt (Fork u a (Just c) o) = Fork u a (Just $ wt <> c) o
prependChild _ _                      = error "prependChild"

balance :: WidgetTree a -> WidgetTree a
balance = fromList' . toList'
  where
    toList' :: WidgetTree a -> [(a, Maybe (WidgetTree a))]
    toList' Null            = []
    toList' (Fork u a mc o) = toList' u ++ (a, balance <$> mc) : toList' o

    fromList' :: [(a, Maybe (WidgetTree a))] -> WidgetTree a
    fromList' [] = Null
    fromList' es = Fork us' a mc os'
      where
        n = length es
        (us, (a,mc):os) = splitAt (n `div` 2) es
        us' = fromList' us
        os' = fromList' os

instance Functor WidgetTree where
  fmap _ Null            = Null
  fmap f (Fork u a mc o) = Fork (fmap f u) (f a) (fmap f <$> mc) (fmap f o)

instance Foldable WidgetTree where
  foldMap _ Null            = mempty
  foldMap f (Fork u a mc o) = foldMap f u <> f a <> maybe mempty (foldMap f) mc <> foldMap f o

instance Traversable WidgetTree where
  traverse _ Null                  = pure Null
  traverse f (Fork u a Nothing o)  =
    Fork <$> traverse f u <*> f a <*> pure Nothing <*> traverse f o
  traverse f (Fork u a (Just c) o) =
    Fork <$> traverse f u <*> f a <*> (Just <$> traverse f c) <*> traverse f o

-- * Path

data SimpleCrumb
  = SUnder
  | SChild
  | SOver
  deriving (Show, Eq, Read)

type WidgetTreePath = [SimpleCrumb]

wtPath :: WidgetTree a -> WidgetTree (a, WidgetTreePath)
wtPath = go []
  where
    go bs Null            = Null
    go bs (Fork u a mc o) = Fork u' (a, reverse bs) mc' o'
      where
        u'  = go (SUnder:bs) u
        mc' = go (SChild:bs) <$> mc
        o'  = go (SOver:bs) o

wtModifyAt :: WidgetTreePath -> (a -> a) -> WidgetTree a -> WidgetTree a
wtModifyAt bs0 f = go bs0
  where
    go _ Null                      = Null
    go [] (Fork u a mc o)          = Fork u (f a) mc o
    go (SUnder:bs) (Fork u a mc o) = Fork (go bs u) a mc o
    go (SChild:bs) (Fork u a mc o) = Fork u a (go bs <$> mc) o
    go (SOver:bs)  (Fork u a mc o) = Fork u a mc (go bs o)

wtAt :: WidgetTreePath -> WidgetTree a -> Maybe a
wtAt = go
  where
    go _  Null                     = Nothing
    go [] (Fork _ a _ _)           = Just a
    go (SUnder:bs) (Fork u _ _ _)  = go bs u
    go (SChild:bs) (Fork _ _ mc _) = go bs =<< mc
    go (SOver:bs) (Fork _ _ _ o)   = go bs o

-- * Zipper

data Crumb a
  = Under a (Maybe (WidgetTree a)) (WidgetTree a)
  | Children (WidgetTree a) a (WidgetTree a)
  | Over (WidgetTree a) (Maybe (WidgetTree a)) a
  deriving (Eq, Show, Read)

type Crumbs a = [Crumb a]

type Zipper a = (WidgetTree a, Crumbs a)

goUnder :: Zipper a -> Maybe (Zipper a)
goUnder (Null, _)           = Nothing
goUnder (Fork u a mc o, bs) = Just (u, Under a mc o : bs)

goChild :: Zipper a -> Maybe (Zipper a)
goChild (Null, _)           = Nothing
goChild (Fork u a mc o, bs) =
  case mc of
    Nothing -> Nothing
    Just c  -> Just (c, Children u a o : bs)

goOver :: Zipper a -> Maybe (Zipper a)
goOver (Null, _)           = Nothing
goOver (Fork u a mc o, bs) = Just (o, Over u mc a : bs)

goUp :: Zipper a -> Zipper a
goUp (tr, [])               = (tr, [])
goUp (u, Under a mc o:bs)   = (Fork u a mc o, bs)
goUp (c, Children u a o:bs) = (Fork u a (Just c) o, bs)
goUp (o, Over u mc a:bs)    = (Fork u a mc o, bs)

topMost :: Zipper a -> Zipper a
topMost z@(_, []) = z
topMost z         = topMost $ goUp z

toZipper :: WidgetTree a -> Zipper a
toZipper tr = (tr, [])

fromZipper :: Zipper a -> WidgetTree a
fromZipper = fst . topMost

focusBy :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
focusBy match = go . topMost
  where
    go (Null, _) = Nothing
    go z@(Fork _ a _ _, _)
      | match a   = Just z
      | otherwise = firstJust id [go =<< goUnder z, go =<< goChild z, go =<< goOver z]
