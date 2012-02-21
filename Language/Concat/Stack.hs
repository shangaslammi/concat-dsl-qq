{-# LANGUAGE TypeOperators, NoImplicitPrelude, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}

module Language.Concat.Stack where

import qualified Data.List as L
import Control.Monad
import Prelude (Bool, Int, Float, fst, snd, flip, ($), uncurry, Show(..), String, (.), otherwise, IO)
import qualified Prelude as P

data s :. a = !s :. !a
infixl 2 :.

instance (Show s, Show a) => Show (s :. a) where
    show (s :. a) = show s P.++ " :. " P.++ show a

instance (P.Eq s, P.Eq a) => P.Eq (s :. a) where
    (s:.a) == (s' :. a') = a P.== a' P.&& s P.== s'

nop :: s -> s
nop = P.id

push :: a -> s -> s:.a
push a s = s:.a

dup :: s:.a -> s:.a:.a
dup (s:.a) = (s:.a:.a)

swap :: s:.a:.b -> s:.b:.a
swap (s:.a:.b) = s:.b:.a

liftS :: (a -> b) -> (s:.a -> s:.b)
liftS f (s:.a) = s:.f a

liftS2 :: (a -> b -> c) -> (s:.a:.b -> s:.c)
liftS2 f (s:.a:.b) = s:.f a b

null :: s:.[a] -> s:.Bool
null = liftS P.null

decons :: s:.[a] -> s:.[a]:.a
decons (s:.(x:xs)) = s:.xs:.x

cons :: s:.[a]:.a -> s:.[a]
cons (s:.xs:.x) = s:. x:xs

drop :: s:.a -> s
drop (s:.a) = s

drop2 :: s:.a:.b -> s
drop2 = drop . drop

drop3 :: s:.a:.b:.c -> s
drop3 = drop . drop2

nip :: s:.a:.b -> s:.b
nip (s:.a:.b) = s:.b

nip2 :: s:.a:.b:.c -> s:.c
nip2 = nip . nip

over :: s:.a:.b -> s:.a:.b:.a
over (s:.a:.b) = (s:.a:.b:.a)

pick :: s:.a:.b:.c -> s:.a:.b:.c:.a
pick (s:.a:.b:.c) = (s:.a:.b:.c:.a)

dupd :: s:.a:.b -> s:.a:.a:.b
dupd (s:.a:.b) = (s:.a:.a:.b)

swapd :: s:.a:.b:.c -> s:.b:.a:.c
swapd (s:.a:.b:.c) = s:.b:.a:.c

rotl :: s:.a:.b:.c -> s:.b:.c:.a
rotl (s:.a:.b:.c) = s:.b:.c:.a

rotr :: s:.a:.b:.c -> s:.c:.a:.b
rotr (s:.a:.b:.c) = s:.c:.a:.b

call :: s:.(s -> s') -> s'
call (s:.f) = f s

dip :: s:.a:.(s -> s') -> s':.a
dip (s:.a:.f) = f s :. a

dip2 :: s:.a:.b:.(s -> s') -> s':.a:.b
dip2 (s:.a:.b:.f) = f s :. a :. b

keep :: s:.a:.(s:.a -> s') -> s':.a
keep (s:.a:.f) = f (s:.a) :. a

keep2 :: s:.a:.b:.(s:.a:.b -> s') -> s':.a:.b
keep2 (s:.a:.b:.f) = f (s:.a:.b) :. a :. b

length :: s:.[a] -> s:.Int
length = liftS P.length

reverse :: s:.[a] -> s:.[a]
reverse = liftS P.reverse

take :: s:.[a]:.Int -> s:.[a]
take = liftS2 $ flip P.take

if_ :: s:.Bool:.(s -> s'):.(s -> s') -> s'
if_ (s:.cond:.then_:.else_)
    | cond      = then_ s
    | otherwise = else_ s

map :: s:.[a]:.(s:.a -> s:.b) -> s:.[b]
map (s:.lst:.f) = (uncurry (:.)) (L.mapAccumL step s lst) where
    step s x = let s:.y = f (s:.x) in (s,y)

putStr :: s:.String -> IO s
putStr (s:.x) = P.putStr x >> return s

putStrLn :: s:.String -> IO s
putStrLn (s:.x) = P.putStrLn x >> return s

getLine :: s -> IO (s:.String)
getLine s = P.getLine >>= \ln -> return (s:.ln)

class BuildList s a | s -> a where
    buildList :: s -> [a]

instance BuildList () a where
    buildList () = []

instance BuildList s a => BuildList (s:.a) a where
    buildList (s:.a) = a : buildList s

list :: BuildList s' a => s :. (() -> s') -> s :. [a]
list (s:.q) = s :. (P.reverse $ buildList (q ()))
