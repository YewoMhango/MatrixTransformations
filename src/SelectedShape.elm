module SelectedShape exposing (..)

import Polygon exposing (Polygon)


type SelectedShape
    = F
    | Arrow


validShapes : List String
validShapes =
    [ "F", "Arrow" ]


fShapedPolygon : Polygon
fShapedPolygon =
    { start = ( 0, 0 )
    , others =
        [ ( 1, 0 )
        , ( 1, 2 )
        , ( 2, 2 )
        , ( 2, 3 )
        , ( 1, 3 )
        , ( 1, 4 )
        , ( 3, 4 )
        , ( 3, 5 )
        , ( 0, 5 )
        ]
    }


arrowShapedPolygon : Polygon
arrowShapedPolygon =
    { start = ( 2, 0 )
    , others =
        [ ( 2, 5 )
        , ( 0, 5 )
        , ( 3, 8 )
        , ( 6, 5 )
        , ( 4, 5 )
        , ( 4, 0 )
        ]
    }


getShapePolygon : SelectedShape -> Polygon
getShapePolygon shape =
    case shape of
        F ->
            fShapedPolygon

        Arrow ->
            arrowShapedPolygon


mapShapeNameToShape : String -> Maybe SelectedShape
mapShapeNameToShape shapeName =
    case shapeName of
        "F" ->
            Just F

        "Arrow" ->
            Just Arrow

        _ ->
            Nothing


mapShapeToShapeName : SelectedShape -> String
mapShapeToShapeName shapeName =
    case shapeName of
        F ->
            "F"

        Arrow ->
            "Arrow"
