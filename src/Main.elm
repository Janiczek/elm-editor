module Main exposing (..)

import Array.Hamt as Array exposing (Array)
import Dom
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD exposing (Decoder)
import Task


main : Program Never Model Msg
main =
    H.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { lines : Array String
    , position : Position
    }


type alias Position =
    { line : Int
    , column : Int
    }


type Msg
    = NoOp
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | NewLine
    | InsertChar Char
    | RemoveCharBefore
    | RemoveCharAfter


initModel : Model
initModel =
    { lines = Array.fromList [ "" ]
    , position = Position 0 0
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Dom.focus "editor"
        |> Task.attempt (always NoOp)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


keyDecoder : Decoder Msg
keyDecoder =
    JD.field "key" JD.string
        |> JD.andThen keyToMsg


keyToMsg : String -> Decoder Msg
keyToMsg string =
    case String.uncons string of
        Just ( char, "" ) ->
            JD.succeed (InsertChar char)

        _ ->
            case string of
                "ArrowUp" ->
                    JD.succeed MoveUp

                "ArrowDown" ->
                    JD.succeed MoveDown

                "ArrowLeft" ->
                    JD.succeed MoveLeft

                "ArrowRight" ->
                    JD.succeed MoveRight

                "Backspace" ->
                    JD.succeed RemoveCharBefore

                "Delete" ->
                    JD.succeed RemoveCharAfter

                "Enter" ->
                    JD.succeed NewLine

                _ ->
                    JD.fail "This key does nothing"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MoveUp ->
            ( { model | position = moveUp model.position model.lines }
            , Cmd.none
            )

        MoveDown ->
            ( { model | position = moveDown model.position model.lines }
            , Cmd.none
            )

        MoveLeft ->
            ( { model | position = moveLeft model.position model.lines }
            , Cmd.none
            )

        MoveRight ->
            ( { model | position = moveRight model.position model.lines }
            , Cmd.none
            )

        NewLine ->
            ( newLine model
            , Cmd.none
            )

        InsertChar char ->
            ( insertChar char model
            , Cmd.none
            )

        RemoveCharBefore ->
            ( removeCharBefore model
            , Cmd.none
            )

        RemoveCharAfter ->
            ( removeCharAfter model
            , Cmd.none
            )


newLine : Model -> Model
newLine { position, lines } =
    let
        { line, column } =
            position

        linesList : List String
        linesList =
            Array.toList lines

        line_ : Int
        line_ =
            line + 1

        contentUntilCursor : List String
        contentUntilCursor =
            linesList
                |> List.take line_
                |> List.indexedMap
                    (\i content ->
                        if i == line then
                            String.left column content
                        else
                            content
                    )

        restOfLineAfterCursor : String
        restOfLineAfterCursor =
            String.dropLeft column (lineContent lines line)

        restOfLines : List String
        restOfLines =
            List.drop line_ linesList

        newLines : Array String
        newLines =
            (contentUntilCursor
                ++ [ restOfLineAfterCursor ]
                ++ restOfLines
            )
                |> Array.fromList

        newPosition : Position
        newPosition =
            { line = line_
            , column = 0
            }
    in
    { lines = newLines
    , position = newPosition
    }


insertChar : Char -> Model -> Model
insertChar char { position, lines } =
    let
        { line, column } =
            position

        lineWithCharAdded : String -> String
        lineWithCharAdded content =
            String.left column content
                ++ String.fromChar char
                ++ String.dropLeft column content

        newLines : Array String
        newLines =
            lines
                |> Array.indexedMap
                    (\i content ->
                        if i == line then
                            lineWithCharAdded content
                        else
                            content
                    )

        newPosition : Position
        newPosition =
            { line = line
            , column = column + 1
            }
    in
    { lines = newLines
    , position = newPosition
    }


removeCharBefore : Model -> Model
removeCharBefore ({ position, lines } as model) =
    if isStartOfDocument position then
        model
    else
        let
            { line, column } =
                position

            lineIsEmpty : Bool
            lineIsEmpty =
                lineContent lines line
                    |> String.isEmpty

            removeCharFromLine : ( Int, String ) -> List String
            removeCharFromLine ( lineNum, content ) =
                if lineNum == line - 1 then
                    if isFirstColumn column then
                        [ content ++ lineContent lines line ]
                    else
                        [ content ]
                else if lineNum == line then
                    if isFirstColumn column then
                        []
                    else
                        [ String.left (column - 1) content
                            ++ String.dropLeft column content
                        ]
                else
                    [ content ]

            newLines : Array String
            newLines =
                lines
                    |> Array.toIndexedList
                    |> List.concatMap removeCharFromLine
                    |> Array.fromList
        in
        { lines = newLines
        , position = moveLeft position lines
        }


removeCharAfter : Model -> Model
removeCharAfter ({ position, lines } as model) =
    if isEndOfDocument lines position then
        model
    else
        let
            { line, column } =
                position

            isOnLastColumn : Bool
            isOnLastColumn =
                isLastColumn lines line column

            removeCharFromLine : ( Int, String ) -> List String
            removeCharFromLine ( lineNum, content ) =
                if lineNum == line then
                    if isOnLastColumn then
                        [ content ++ lineContent lines (line + 1) ]
                    else
                        [ String.left column content
                            ++ String.dropLeft (column + 1) content
                        ]
                else if lineNum == line + 1 then
                    if isOnLastColumn then
                        []
                    else
                        [ content ]
                else
                    [ content ]

            newLines : Array String
            newLines =
                lines
                    |> Array.toIndexedList
                    |> List.concatMap removeCharFromLine
                    |> Array.fromList
        in
        { lines = newLines
        , position = position
        }


moveUp : Position -> Array String -> Position
moveUp { line, column } lines =
    if isFirstLine line then
        startOfDocument
    else
        let
            line_ : Int
            line_ =
                previousLine line
        in
        { line = line_
        , column = clampColumn lines line_ column
        }


moveDown : Position -> Array String -> Position
moveDown { line, column } lines =
    if isLastLine lines line then
        endOfDocument lines
    else
        let
            line_ : Int
            line_ =
                nextLine lines line
        in
        { line = line_
        , column = clampColumn lines line_ column
        }


moveLeft : Position -> Array String -> Position
moveLeft ({ line, column } as position) lines =
    if isStartOfDocument position then
        position
    else if isFirstColumn column then
        let
            line_ : Int
            line_ =
                previousLine line
        in
        { line = line_
        , column = lastColumn lines line_
        }
    else
        { line = line
        , column = column - 1
        }


moveRight : Position -> Array String -> Position
moveRight ({ line, column } as position) lines =
    if isEndOfDocument lines position then
        position
    else if isLastColumn lines line column then
        { line = nextLine lines line
        , column = 0
        }
    else
        { line = line
        , column = column + 1
        }


startOfDocument : Position
startOfDocument =
    { line = 0
    , column = 0
    }


endOfDocument : Array String -> Position
endOfDocument lines =
    { line = lastLine lines
    , column = lastColumn lines (lastLine lines)
    }


isStartOfDocument : Position -> Bool
isStartOfDocument { line, column } =
    isFirstLine line
        && isFirstColumn column


isEndOfDocument : Array String -> Position -> Bool
isEndOfDocument lines { line, column } =
    isLastLine lines line
        && isLastColumn lines line column


isFirstLine : Int -> Bool
isFirstLine line =
    line == 0


isLastLine : Array String -> Int -> Bool
isLastLine lines line =
    line == lastLine lines


isFirstColumn : Int -> Bool
isFirstColumn column =
    column == 0


isLastColumn : Array String -> Int -> Int -> Bool
isLastColumn lines line column =
    column == lastColumn lines line


lastLine : Array String -> Int
lastLine lines =
    Array.length lines - 1


previousLine : Int -> Int
previousLine line =
    (line - 1)
        |> max 0


nextLine : Array String -> Int -> Int
nextLine lines line =
    (line + 1)
        |> min (maxLine lines)


maxLine : Array String -> Int
maxLine lines =
    Array.length lines - 1


lastColumn : Array String -> Int -> Int
lastColumn lines line =
    lineLength lines line


clampColumn : Array String -> Int -> Int -> Int
clampColumn lines line column =
    column
        |> clamp 0 (lineLength lines line)


lineContent : Array String -> Int -> String
lineContent lines lineNum =
    lines
        |> Array.get lineNum
        |> Maybe.withDefault ""


lineLength : Array String -> Int -> Int
lineLength lines lineNum =
    lineContent lines lineNum
        |> String.length


view : Model -> Html Msg
view model =
    H.div []
        [ viewEditor model
        , viewDebug model
        ]


viewDebug : Model -> Html Msg
viewDebug { lines, position } =
    H.div
        [ HA.style [ ( "max-width", "100%" ) ] ]
        [ H.text "lines:"
        , H.pre
            [ HA.style [ ( "white-space", "pre-wrap" ) ] ]
            [ H.text (toString lines) ]
        , H.text "position:"
        , H.pre [] [ H.text (toString position) ]
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    H.div
        [ HA.style
            [ ( "display", "flex" )
            , ( "flex-direction", "row" )
            , ( "font-family", "monospace" )
            , ( "font-size", toString fontSize ++ "px" )
            , ( "line-height", toString lineHeight ++ "px" )
            , ( "white-space", "pre" )
            ]
        , HE.on "keydown" keyDecoder
        , HA.tabindex 0
        , HA.id "editor"
        ]
        [ viewLineNumbers model
        , viewContent model
        ]


viewLineNumbers : Model -> Html Msg
viewLineNumbers model =
    H.div
        [ HA.style
            [ ( "width", "2em" )
            , ( "text-align", "center" )
            , ( "color", "#888" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            ]
        ]
        (List.range 1 (Array.length model.lines)
            |> List.map viewLineNumber
        )


viewLineNumber : Int -> Html Msg
viewLineNumber n =
    H.span [] [ H.text (toString n) ]


viewContent : Model -> Html Msg
viewContent model =
    H.div
        [ HA.style
            [ ( "position", "relative" )
            , ( "flex", "1" )
            , ( "background-color", "#f0f0f0" )
            ]
        ]
        [ viewLines model.position model.lines ]


viewLines : Position -> Array String -> Html Msg
viewLines position lines =
    H.div []
        (lines
            |> Array.indexedMap (viewLine position lines)
            |> Array.toList
        )


viewLine : Position -> Array String -> Int -> String -> Html Msg
viewLine position lines lineNum line =
    H.div
        [ HA.style
            [ ( "position", "absolute" )
            , ( "left", "0" )
            , ( "top", toString (toFloat lineNum * lineHeight) ++ "px" )
            ]
        ]
        (if position.line == lineNum then
            if isLastColumn lines lineNum position.column then
                [ H.text line
                , viewCursor nbsp
                ]
            else
                [ H.text (String.left position.column line)
                , viewCursor (String.left 1 (String.dropLeft position.column line))
                , H.text (String.dropLeft (position.column + 1) line)
                ]
         else
            [ H.text line ]
        )


nbsp : String
nbsp =
    " "


viewCursor : String -> Html Msg
viewCursor char =
    H.span
        [ HA.style [ ( "background-color", "orange" ) ] ]
        [ H.text char ]


fontSize : Float
fontSize =
    20


lineHeight : Float
lineHeight =
    fontSize * 1.2
