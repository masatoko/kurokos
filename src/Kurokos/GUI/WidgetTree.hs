module Kurokos.GUI.WidgetTree where

import           Data.Monoid ((<>))

data WidgetTree a
  = Null
  | Single (WidgetTree a) a (WidgetTree a)
  | Container (WidgetTree a) a (WidgetTree a) (WidgetTree a)
  deriving Show

showTree :: Show a => WidgetTree a -> String
showTree = unlines . work 0
  where
    work _ Null = []
    work n (Single u a o) =
      work (n + 1) u
        ++ [indent ++ "+ " ++ show a]
        ++ work (n + 1) o
      where
        indent = replicate (4 * n) ' '
    work n (Container u a c o) =
      work (n + 1) u
        ++ [indent ++ "@ " ++ show a]
        ++ work (n + 1) c
        ++ work (n + 1) o
      where
        indent = replicate (4 * n) ' '

pretty :: Show a => WidgetTree a -> String
pretty = unlines . work 0
  where
    work _ Null = []
    work n (Single u a o) =
      work n u ++ [indent ++ "+ " ++ show a] ++ work n o
      where
        indent = replicate (2 * n) ' '
    work n (Container u _a c o) =
      work n u ++ [indent ++ "@ | Container"] ++ work (n+1) c ++ work n o
      where
        indent = replicate (2 * n) ' '

size :: WidgetTree a -> Int
size Null                = 0
size (Single u _ o)      = size u + 1 + size o
size (Container u _ c o) = size u + 1 + size c + size o

prepend :: a -> WidgetTree a -> WidgetTree a
prepend k Null                = Single Null k Null
prepend k (Single u a o)      = Single (prepend k u) a o
prepend k (Container u a c o) = Container (prepend k u) a c o

append :: WidgetTree a -> a -> WidgetTree a
append Null k                = Single Null k Null
append (Single u a o) k      = Single u a (append o k)
append (Container u a c o) k = Container u a c (append o k)

prependC :: a -> WidgetTree a -> WidgetTree a
prependC k Null                = Container Null k Null Null
prependC k (Single u a o)      = Single (prependC k u) a o
prependC k (Container u a c o) = Container (prependC k u) a c o

appendC :: WidgetTree a -> a -> WidgetTree a
appendC Null k                = Container Null k Null Null
appendC (Single u a o) k      = Single u a (appendC o k)
appendC (Container u a c o) k = Container u a c (appendC o k)

wtappend :: WidgetTree a -> WidgetTree a -> WidgetTree a
wtappend a Null                 = a
wtappend Null a                 = a
wtappend wt (Single u a o)      = Single (wt `wtappend` u) a o
wtappend wt (Container u a c o) = Container (wt `wtappend` u) a c o

wtFromList :: [a] -> WidgetTree a
wtFromList [] = Null
wtFromList as = Single (wtFromList us) a (wtFromList os)
  where
    k = length as `div` 2
    (us, a:os) = splitAt k as

wtconcat :: Foldable f => f (WidgetTree a) -> WidgetTree a
wtconcat = foldl1 wtappend

appendChild :: WidgetTree a -> WidgetTree a -> Maybe (WidgetTree a)
appendChild wt (Container u a c o) = Just $ Container u a (wt `wtappend` c) o
appendChild _ _                    = Nothing

prependChild :: WidgetTree a -> WidgetTree a -> Maybe (WidgetTree a)
prependChild (Container u a c o) wt = Just $ Container u a (c `wtappend` wt) o
prependChild _ _                    = Nothing

toList :: WidgetTree a -> [a]
toList Null                = []
toList (Single u a o)      = toList u ++ a : toList o
toList (Container u a c o) = toList u ++ a : toList c ++ toList o

balance :: WidgetTree a -> WidgetTree a
balance = fromList' . toList'
  where
    toList' :: WidgetTree a -> [(a, Maybe (WidgetTree a))]
    toList' Null                = []
    toList' (Single u a o)      = toList' u ++ (a, Nothing) : toList' o
    toList' (Container u a c o) = toList' u ++ (a, Just (balance c)) : toList' o

    fromList' :: [(a, Maybe (WidgetTree a))] -> WidgetTree a
    fromList' [] = Null
    fromList' es = work x
      where
        n = length es
        (us, x:os) = splitAt (n `div` 2) es
        us' = fromList' us
        os' = fromList' os

        work (a, Just wt) = Container us' a wt os'
        work (a, Nothing) = Single us' a os'


instance Functor WidgetTree where
  fmap _ Null                = Null
  fmap f (Single u a o)      = Single (fmap f u) (f a) (fmap f o)
  fmap f (Container u a c o) = Container (fmap f u) (f a) (fmap f c) (fmap f o)

instance Foldable WidgetTree where
  foldMap _ Null                = mempty
  foldMap f (Single u a o)      = foldMap f u <> f a <> foldMap f o
  foldMap f (Container u a c o) = foldMap f u <> f a <> foldMap f c <> foldMap f o

instance Traversable WidgetTree where
  traverse _ Null = pure Null
  traverse f (Single u a o) = Single <$> traverse f u <*> f a <*> traverse f o
  traverse f (Container u a c o) = Container <$> traverse f u <*> f a <*> traverse f c <*> traverse f o
