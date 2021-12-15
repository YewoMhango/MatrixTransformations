module Matrix exposing
    ( Matrix
    , addMatrices
    , getHeight
    , getValue
    , getWidth
    , multiplyMatrices
    , newMatrix
    , newMatrixWithDefault
    , setValue
    , subtractMatrices
    , withIdentityDefault
    )

import Array exposing (Array)
import Dict exposing (values)


type Matrix
    = Matrix
        { width : Int
        , height : Int
        , values : Array Float
        }


newMatrix : Int -> Int -> List Float -> Maybe Matrix
newMatrix width height values =
    if width < 1 || height < 1 then
        Nothing

    else if List.length values == width * height then
        Just <|
            Matrix
                { width = width
                , height = height
                , values = Array.fromList values
                }

    else
        Nothing


newMatrixWithDefault : Int -> Int -> List Float -> Matrix
newMatrixWithDefault width height values =
    withIdentityDefault <|
        newMatrix width height values


withIdentityDefault : Maybe Matrix -> Matrix
withIdentityDefault maybeMatrix =
    Maybe.withDefault
        (Matrix
            { width = 1
            , height = 1
            , values = Array.fromList [ 1 ]
            }
        )
        maybeMatrix


getWidth : Matrix -> Int
getWidth matrix =
    case matrix of
        Matrix m ->
            m.width


getHeight : Matrix -> Int
getHeight matrix =
    case matrix of
        Matrix m ->
            m.height


getValue : Int -> Int -> Matrix -> Maybe Float
getValue x y matrix =
    case matrix of
        Matrix m ->
            Array.get (x + y * m.width) m.values


setValue : Int -> Int -> Float -> Matrix -> Matrix
setValue x y value matrix =
    case matrix of
        Matrix m ->
            Matrix
                { m
                    | values =
                        Array.set
                            (x + y * m.width)
                            value
                            m.values
                }


mapMatrices : (Float -> Float -> Float) -> Matrix -> Matrix -> Maybe Matrix
mapMatrices function first second =
    case first of
        Matrix f ->
            case second of
                Matrix s ->
                    if f.width /= s.width || f.height /= s.height then
                        Nothing

                    else
                        Just <|
                            Matrix
                                { f
                                    | values =
                                        Array.indexedMap
                                            (\idx value ->
                                                Array.get idx s.values
                                                    |> Maybe.withDefault 0
                                                    |> function value
                                            )
                                            f.values
                                }


addMatrices : Matrix -> Matrix -> Maybe Matrix
addMatrices =
    mapMatrices (+)


subtractMatrices : Matrix -> Matrix -> Maybe Matrix
subtractMatrices =
    mapMatrices (-)


multiplyMatrices : Matrix -> Matrix -> Maybe Matrix
multiplyMatrices first second =
    case first of
        Matrix f ->
            case second of
                Matrix s ->
                    if f.width /= s.height then
                        Nothing

                    else
                        let
                            resultingWidth =
                                s.width

                            resultingHeight =
                                f.height
                        in
                        Just <|
                            Matrix
                                { width = resultingWidth
                                , height = resultingHeight
                                , values =
                                    for 0
                                        resultingHeight
                                        (\j ->
                                            for 0
                                                resultingWidth
                                                (\i ->
                                                    for 0
                                                        f.width
                                                        (\n ->
                                                            Maybe.withDefault 1 (getValue n j <| Matrix f)
                                                                * Maybe.withDefault 1 (getValue i n <| Matrix s)
                                                        )
                                                        |> List.foldl (+) 0
                                                )
                                        )
                                        |> List.foldr (++) []
                                        |> Array.fromList
                                }


{-|

    Can be used to map over a range like a "for"
    loop in some imperative languages, but returns
    a result-value list. It stops at `end - 1`.

-}
for : Int -> Int -> (Int -> a) -> List a
for curr end func =
    if curr == end - 1 then
        [ func curr ]

    else
        func curr :: for (curr + 1) end func
