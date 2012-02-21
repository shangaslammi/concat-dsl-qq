{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeOperators #-}

import Test.HUnit
import Test.Hspec
import Test.Hspec.HUnit

import Prelude hiding (drop, foldl, null)

import Language.Concat

specs = describe "Concatenative DSL" $
    [ it "should treat an empty program as the stack identity function" $ do
        let prog :: s -> s
            prog = [cc| |]
        prog () @?= ()
        prog (():.1) @?= () :. 1

    , it "should implicitly push a lone literal value into the stack" $ do
        let prog :: s -> s:.Int
            prog = [cc| 1 |]
        prog () @?= ():.1
        prog (():."foo") @?= () :. "foo" :. 1

    , it "should push consecutive literals into the stack" $ do
        let prog :: s -> s:.Int:.String:.Int
            prog = [cc| 1 "foo" 3 |]
        prog () @?= ():.1:."foo":.3

    , it "should apply named stack functions to the current stack" $ do
        let prog :: s -> s:.Int:.Int
            prog = [cc| 1 dup |]
        prog () @?= () :. 1 :. 1

    , it "applies infix operators to the top two elements of the stack" $ do
        let prog :: s -> s:.Int
            prog = [cc| 1 2 (+) |]
        prog () @?= () :. 3

    , it "should push operations in square brackets to the stack without executing them" $ do
        let prog :: s -> s:.Int
            prog = [cc| 1 [2 (+)] call |]
        prog () @?= () :. 3

    , it "should turn quotations into lists with the 'list' word" $ do
        let prog :: s -> s:.[Int]
            prog = [cc| [1 2 dup] list |]
        prog () @?= () :. [1, 2, 2]

        let prog2 :: s:.String -> s:.String
            prog2 = [cc| ['b' 'a' 'r'] list (++) |]
        prog2 (() :. "foo") @?= () :. "foobar"

    , it "should support if-then-else syntax" $ do
        let prog = [cc| if [dup 0 (>)] then [1 (-)] else [] |]
        prog (() :. 5) @?= () :. 4
        prog (() :. 0) @?= () :. 0

    ]

foldl = [cc|
    pick null
    [drop nip]
    [ [decons swap] dip2 rotl
      [[call] keep] dip rotr foldl
    ] if_
|]

testFold = [cc| [1 2 3 4 5] list 0 [(+)] foldl |]

main = hspec specs
