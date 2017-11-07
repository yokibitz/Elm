-- {- OVERVIEW ------------------------------------------------------
-- A "Tree" represents a binary tree. A "Node" in a binary tree
-- always has two children. A tree can also be "Empty". Below I have
-- defined "Tree" and a number of useful functions.
-- This example also includes some challenge problems!
-- -----------------------------------------------------------------}


module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


-- TREES


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


empty : Tree a
empty =
    Empty


singleton : a -> Tree a
singleton v =
    Node v Empty Empty


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
        Empty ->
            singleton x

        Node y left right ->
            if x > y then
                Node y left (insert x right)
            else if x < y then
                Node y (insert x left) right
            else
                tree


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


depth : Tree a -> Int
depth tree =
    case tree of
        Empty ->
            0

        Node v left right ->
            1 + max (depth left) (depth right)


depth_ : Tree a -> Int
depth_ tree =
    case tree of
        Empty ->
            0

        Node v left right ->
            fold (\_ init -> 1 + max (depth_ left) (depth_ right)) 0 tree


map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
        Empty ->
            Empty

        Node v left right ->
            Node (f v) (map f left) (map f right)


map_ f =
    fold (f >> insert) Empty



-- map_ f =
--     fold (f >> (::)) []
-- fold : (a -> b -> b) -> b -> Tree a -> b
-- fold f b tree =
--     case tree of
--         Empty ->
--             b
--         Node h left right ->
--             fold f (fold f (f h b) left) right


sum : Tree number -> number
sum tree =
    case tree of
        Empty ->
            0

        Node h left right ->
            h + sum left + sum right


sum_ : Tree number -> number
sum_ =
    fold (+) 0


flatten : Tree a -> List a
flatten tree =
    case tree of
        Empty ->
            []

        Node h left right ->
            [ h ] ++ flatten left ++ flatten right


flatten_ : Tree a -> List a
flatten_ tree =
    fold (::) [] tree


olFlatten : Tree a -> List a
olFlatten =
    fold (\a -> \b -> b ++ [ a ]) []


isElement : a -> Tree a -> Bool
isElement a tree =
    case tree of
        Empty ->
            False

        Node h left right ->
            h == a || isElement a left || isElement a right


isElement_ : a -> Tree a -> Bool
isElement_ a =
    fold (\item -> \b -> item == a || b) False


isElement__ : a -> Tree a -> Bool
isElement__ a =
    fold ((==) a >> (||)) False


fold : (a -> b -> b) -> b -> Tree a -> b
fold f b tree =
    case tree of
        Empty ->
            b

        Node h left right ->
            fold f (fold f (f h b) left) right


multiply : Tree number -> number
multiply tree =
    fold (*) 1 tree



-- PLAYGROUND


deepTree =
    fromList [ 1, 2, 3 ]


leftDeepTree =
    fromList [ 3, 2, 1 ]


niceTree =
    fromList [ 2, 1, 3 ]


main =
    div [ style [ ( "font-family", "monospace" ) ] ]
        [ display "depth deepTree" (depth deepTree)
        , display "depth_ deepTree" (depth_ deepTree)
        , display "depth niceTree" (depth niceTree)
        , display "depth leftDeepTree" (depth leftDeepTree)
        , display "incremented" (map (\n -> n + 1) niceTree)
        , display "incremented map_" (map_ (\n -> n + 1) niceTree)
        , display "deepTree flatten" (flatten niceTree)
        , display "deepTree flatten_" (flatten_ niceTree)
        , display "deepTree olFlatter" (olFlatten niceTree)
        , display "sum deepTree" (sum deepTree)
        , display "sum_ deepTree" (sum_ deepTree)
        , display "is 2 element of deepTree" (isElement 2 deepTree)
        , display "is 5 element of deepTree" (isElement 5 deepTree)
        , display "is 2 element_ of deepTree" (isElement_ 2 deepTree)
        , display "is 5 element_ of deepTree" (isElement_ 5 deepTree)
        , display "is 2 element__ of deepTree" (isElement__ 2 deepTree)
        , display "is 5 element__ of deepTree" (isElement__ 5 deepTree)
        , display "multiply contents of deepTree" (multiply deepTree)
        , display "multiply contents of Empty" (multiply Empty)
        ]


display : String -> a -> Html msg
display name value =
    div [] [ text (name ++ " ==> " ++ toString value) ]



-- {-----------------------------------------------------------------
-- Exercises:
-- (1) Sum all of the elements of a tree.
--        sum : Tree number -> number
-- (2) Flatten a tree into a list.
--        flatten : Tree a -> List a
-- (3) Check to see if an element is in a given tree.
--        isElement : a -> Tree a -> Bool
-- (4) Write a general fold function that acts on trees. The fold
--     function does not need to guarantee a particular order of
--     traversal.
--        fold : (a -> b -> b) -> b -> Tree a -> b
-- (5) Use "fold" to do exercises 1-3 in one line each. The best
--     readable versions I have come up have the following length
--     in characters including spaces and function name:
--       sum: 16
--       flatten: 21
--       isElement: 46
--     See if you can match or beat me! Don't forget about currying
--     and partial application!
-- (6) Can "fold" be used to implement "map" or "depth"?
-- (7) Try experimenting with different ways to traverse a
--     tree: pre-order, in-order, post-order, depth-first, etc.
--     More info at: http://en.wikipedia.org/wiki/Tree_traversal
-- -----------------------------------------------------------------}
