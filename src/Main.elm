module Main exposing (..)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Color
import Debug exposing (toString)
import Html exposing (Html, br, div, input, table, td, tr)
import Html.Attributes exposing (class, id, step, type_, value)
import Html.Events exposing (onInput)
import Matrix exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { matrix : Matrix
    , original : Polygon
    , transformed : Polygon
    }


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


originalMatrix : Matrix
originalMatrix =
    newMatrixWithDefault 2 2 [ 1, 0.5, 0, 1 ]


init : Model
init =
    { matrix =
        originalMatrix
    , original =
        fShapedPolygon
    , transformed =
        transformPolygon originalMatrix fShapedPolygon
    }


type Msg
    = ChangedFirst String
    | ChangedSecond String
    | ChangedThird String
    | ChangedFourth String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedFirst value ->
            updateMatrixValue model 0 0 value

        ChangedSecond value ->
            updateMatrixValue model 1 0 value

        ChangedThird value ->
            updateMatrixValue model 0 1 value

        ChangedFourth value ->
            updateMatrixValue model 1 1 value


updateMatrixValue : Model -> Int -> Int -> String -> Model
updateMatrixValue model x y value =
    let
        matrix =
            setValue
                x
                y
                (Maybe.withDefault 0 <|
                    String.toFloat value
                )
                model.matrix
    in
    { model
        | matrix = matrix
        , transformed =
            transformPolygon matrix model.original
    }


canvasWidth : number
canvasWidth =
    450


canvasHeight : number
canvasHeight =
    450


view : Model -> Html Msg
view model =
    div []
        [ table []
            [ tr []
                [ td [ id "matrix-label" ]
                    [ Html.text "Transformation matrix:"
                    ]
                , td []
                    [ table []
                        [ tr []
                            [ renderNumberInput ChangedFirst 0 0 model
                            , renderNumberInput ChangedSecond 1 0 model
                            ]
                        , tr []
                            [ renderNumberInput ChangedThird 0 1 model
                            , renderNumberInput ChangedFourth 1 1 model
                            ]
                        ]
                    ]
                ]
            ]
        , br [] []
        , renderCanvas model
        ]


renderNumberInput : (String -> Msg) -> Int -> Int -> Model -> Html Msg
renderNumberInput msg x y model =
    td []
        [ input
            [ type_ "number"
            , onInput msg
            , step "0.1"
            , value <|
                toString <|
                    Maybe.withDefault 0 <|
                        getValue x y model.matrix
            ]
            []
        ]


renderCanvas : Model -> Html Msg
renderCanvas model =
    let
        bounds =
            getBoundsForMultiplePolygons [ model.original, model.transformed ]

        ySpan =
            bounds.top - bounds.bottom

        xSpan =
            bounds.right - bounds.left

        maxSpan =
            max ySpan xSpan

        scale =
            canvasWidth / (maxSpan * 1.25)

        interval =
            2.0 ^ ((toFloat <| floor <| logBase 2 maxSpan) - 2)

        xMargin =
            (canvasHeight - (scale * xSpan)) / 2

        yMargin =
            (canvasHeight - (scale * ySpan)) / 2

        centerY =
            yMargin + (scale * bounds.top)

        centerX =
            xMargin + (scale * abs bounds.left)

        labelTickLength =
            10
    in
    Canvas.toHtml ( canvasWidth, canvasHeight )
        []
        [ Canvas.clear ( 0, 0 ) canvasWidth canvasHeight
        , renderXLabels interval centerX centerY scale labelTickLength
        , renderYLabels interval centerX centerY scale labelTickLength
        , shapes []
            [ rect ( 0, centerY ) canvasWidth 1
            , rect ( centerX, 0 ) 1 canvasHeight
            ]
        , shapes
            [ fill <| Color.rgba 0.5 1 0 0.3
            , stroke <| Color.rgba 0.5 1 0 0.6
            ]
            [ renderPolygon model.original centerX centerY scale ]
        , shapes
            [ fill <| Color.rgba 0.5 0 1 0.3
            , stroke <| Color.rgba 0.5 0 1 0.6
            ]
            [ renderPolygon model.transformed centerX centerY scale ]
        ]


renderPolygon : Polygon -> Float -> Float -> Float -> Shape
renderPolygon polygon centerX centerY scale =
    let
        convertor =
            cartesianToCanvas centerX centerY scale
    in
    path (convertor polygon.start) <|
        List.map lineTo <|
            List.map convertor polygon.others


cartesianToCanvas : Float -> Float -> Float -> Point -> Point
cartesianToCanvas centerX centerY scale point =
    let
        ( x, y ) =
            point
    in
    ( centerX + scale * x, centerY - scale * y )


transformPolygon : Matrix -> Polygon -> Polygon
transformPolygon matrix polygon =
    { start = transformPoint matrix polygon.start
    , others =
        List.map (transformPoint matrix) polygon.others
    }


transformPoint : Matrix -> Point -> Point
transformPoint matrix point =
    let
        ( x1, y1 ) =
            point

        pointMatrix =
            newMatrixWithDefault 1 2 [ x1, y1 ]

        result =
            withIdentityDefault <| multiplyMatrices matrix pointMatrix
    in
    ( getValue 0 0 result |> Maybe.withDefault 0
    , getValue 0 1 result |> Maybe.withDefault 0
    )


renderXLabels : Float -> Float -> Float -> Float -> Int -> Renderable
renderXLabels interval centerX centerY scale labelTickLength =
    let
        numberOfTicks =
            floor ((canvasWidth / scale) / interval)

        firstNumber =
            -interval * toFloat (floor ((centerX / scale) / interval))

        margin =
            centerX + (scale * firstNumber)

        fontSize =
            12
    in
    group
        []
        (for
            0
            numberOfTicks
            (\i ->
                let
                    x =
                        margin + (toFloat i * interval * scale)

                    label =
                        toString (firstNumber + (toFloat i * interval))
                in
                group [ fill Color.black ]
                    [ shapes []
                        [ rect
                            ( x, centerY - (toFloat labelTickLength / 2) )
                            1
                            (toFloat labelTickLength)
                        ]
                    , text
                        [ font { size = fontSize, family = "Segoe UI" }
                        , align Center
                        ]
                        ( x
                        , centerY + toFloat labelTickLength + 1 + fontSize / 2
                        )
                        (if label == "0" then
                            ""

                         else
                            label
                        )
                    ]
            )
        )


renderYLabels : Float -> Float -> Float -> Float -> Int -> Renderable
renderYLabels interval centerX centerY scale labelTickLength =
    let
        numberOfTicks =
            floor ((canvasHeight / scale) / interval)

        firstNumber =
            interval * toFloat (floor ((centerY / scale) / interval))

        margin =
            centerY - (scale * firstNumber)

        fontSize =
            12
    in
    group
        []
        (for
            0
            numberOfTicks
            (\i ->
                let
                    y =
                        margin + (toFloat i * interval * scale)

                    label =
                        toString (firstNumber - (toFloat i * interval))
                in
                group [ fill Color.black ]
                    [ shapes []
                        [ rect
                            ( centerX - (toFloat labelTickLength / 2), y )
                            (toFloat labelTickLength)
                            1
                        ]
                    , text
                        [ font { size = fontSize, family = "Segoe UI" }
                        , align Left
                        ]
                        ( centerX + toFloat labelTickLength + 1
                        , y + fontSize / 2
                        )
                        (if label == "0" then
                            ""

                         else
                            label
                        )
                    ]
            )
        )


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



-- matrixAsHtml : Matrix -> Html Msg
-- matrixAsHtml matrix =
--     table [ class "matrix" ]
--         (for 0
--             (getHeight matrix)
--             (\h ->
--                 tr []
--                     (for 0
--                         (getWidth matrix)
--                         (\w ->
--                             td []
--                                 [ Html.text <|
--                                     Debug.toString <|
--                                         Maybe.withDefault 0 <|
--                                             Matrix.getValue w h matrix
--                                 ]
--                         )
--                     )
--             )
--         )


for : Int -> Int -> (Int -> a) -> List a
for current end function =
    if current == end - 1 then
        [ function current ]

    else
        function current :: for (current + 1) end function
