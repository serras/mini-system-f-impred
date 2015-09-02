id :: {a} a -> a = \x -> x;

import caseList :: {a} {b} b -> (a -> b -> b) -> b;

import idu :: {a} a -> a;
iduInt :: 'Int -> 'Int = idu;

import f :: {b} ('List b) -> b -> 'Int;
import xs :: 'List ({a} a -> a);

g = f xs (\x -> x);

import app :: {a} {b} (a -> b) -> a -> b;

h = app f xs;

import f2 :: {b} ('List b) -> b -> Int;
import xs2 :: 'List (({c} c -> c) -> 'Int);
import tres :: 'Int;

i = f2 xs2 (\y -> y tres);
