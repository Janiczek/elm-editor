module Tests.InsertChar exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (Msg(InsertChar), lineContent, lineLength)
import Test exposing (..)
import Tests.Common exposing (..)


movesRight : Test
movesRight =
    msgTest "InsertChar moves right"
        app
        insertChar
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.position.column
                |> Expect.equal (modelBeforeMsg.position.column + 1)


doesntMoveUpOrDown : Test
doesntMoveUpOrDown =
    msgTest "InsertChar doesn't move up or down"
        app
        insertChar
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.position.line
                |> Expect.equal modelBeforeMsg.position.line


makesLineLonger : Test
makesLineLonger =
    msgTest "InsertChar makes line longer"
        app
        insertChar
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            lineLength finalModel.lines finalModel.position.line
                |> Expect.equal (lineLength modelBeforeMsg.lines modelBeforeMsg.position.line + 1)


insertsChar : Test
insertsChar =
    msgTest "InsertChar inserts char"
        app
        insertChar
    <|
        \_ _ modelBeforeMsg msg finalModel ->
            case msg of
                InsertChar char ->
                    let
                        charString =
                            String.fromChar char

                        newLine =
                            lineContent finalModel.lines finalModel.position.line

                        oldLine =
                            lineContent modelBeforeMsg.lines modelBeforeMsg.position.line

                        beforeCursor =
                            String.left modelBeforeMsg.position.column oldLine

                        afterCursor =
                            String.dropLeft modelBeforeMsg.position.column oldLine
                    in
                    newLine
                        |> Expect.equal (beforeCursor ++ charString ++ afterCursor)

                _ ->
                    Expect.pass
