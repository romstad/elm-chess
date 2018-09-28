module Internal.Util exposing (failableFoldl)

{-| Like List.foldl, but returns a Maybe value, and exits early when some
application of `f` along the way returns `Nothing`. Here's a function that
computes the sum of a list of integers < 100, but returns `Nothing` when the
list contains at least one number >= 100:

    sumOfSmall : List Int -> Maybe Int
    sumOfSmall =
        failableFoldl
            (\x sum ->
                if x < 100 then
                    Just (x + sum)
                else
                    Nothing
            )
            0

    sumOfSmall [1, 2, 3] == Just 6
    sumOfSmall [1, 2, 103] == Nothing

-}


failableFoldl : (a -> b -> Maybe b) -> b -> List a -> Maybe b
failableFoldl f init =
    List.foldl
        (Maybe.andThen << f)
        (Just init)
