{-# LANGUAGE TemplateHaskell #-}
module Kurokos.UI.WidgetTree where

import           Control.Lens
import           Data.Foldable (toList)
import           Data.Monoid   ((<>))

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

appendChild :: WidgetTree a -> WidgetTree a -> Maybe (WidgetTree a)
appendChild (Fork u a (Just c) o) wt = Just $ Fork u a (Just $ c <> wt) o
appendChild _ _                      = Nothing

prependChild :: WidgetTree a -> WidgetTree a -> Maybe (WidgetTree a)
prependChild wt (Fork u a (Just c) o) = Just $ Fork u a (Just $ wt <> c) o
prependChild _ _                      = Nothing

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
