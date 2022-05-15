module Polygon exposing (..)

import Canvas exposing (Point)


type alias Polygon =
    { start : Point
    , others : List Point
    }


type alias Bounds =
    { left : Float
    , right : Float
    , top : Float
    , bottom : Float
    }


getPolygonBounds : Polygon -> Bounds
getPolygonBounds polygon =
    List.foldl
        (\curr acc ->
            let
                ( x, y ) =
                    curr
            in
            { left = min x acc.left
            , right = max x acc.right
            , top = max y acc.top
            , bottom = min y acc.bottom
            }
        )
        (pointToBounds polygon.start)
        polygon.others


combineTwoBounds : Bounds -> Bounds -> Bounds
combineTwoBounds b1 b2 =
    { left = min b1.left b2.left
    , right = max b1.right b2.right
    , top = max b1.top b2.top
    , bottom = min b1.bottom b2.bottom
    }


getBoundsForMultiplePolygons : List Polygon -> Bounds
getBoundsForMultiplePolygons polygons =
    case polygons of
        first :: rest ->
            List.foldl
                (\polygon bounds ->
                    combineTwoBounds bounds <|
                        getPolygonBounds polygon
                )
                (getPolygonBounds first)
                rest

        [] ->
            pointToBounds ( 0, 0 )


pointToBounds : Point -> Bounds
pointToBounds point =
    let
        ( x, y ) =
            point
    in
    { left = x
    , right = x
    , top = y
    , bottom = y
    }
