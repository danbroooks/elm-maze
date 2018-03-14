module List.Util exposing (..)

import List exposing (..)


shuffle : Int -> List a -> List a
shuffle n xs =
    if n < 1 then
        xs
    else
        case shuffle (n - 1) xs of
            [] ->
                []

            h :: t ->
                reverse (h :: shift n t)


shift : Int -> List a -> List a
shift n xs =
    if n < 1 then
        xs
    else
        case shift (n - 1) xs of
            [] ->
                []

            h :: t ->
                reverse (h :: reverse t)


memberOf : List a -> a -> Bool
memberOf xs x =
    member x xs


reject : (a -> Bool) -> List a -> List a
reject f xs =
    filter (\x -> f x == False) xs


intersection : List a -> List a -> List a
intersection a b =
    filter (memberOf b) a


union : List a -> List a -> List a
union xs ys =
    concat [ xs, reject (memberOf xs) ys ]
