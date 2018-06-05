module Tests.NewLine exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (lineContent)
import Test exposing (..)
import Tests.Common exposing (..)


movesDownALine : Test
movesDownALine =
    msgTest "NewLine moves down a line"
        app
        newLine
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.line
                |> Expect.equal (modelBeforeMsg.cursor.line + 1)


movesToFirstColumn : Test
movesToFirstColumn =
    msgTest "NewLine moves to first column"
        app
        newLine
    <|
        \_ _ _ _ finalModel ->
            finalModel.cursor.column
                |> Expect.equal 0


addsALine : Test
addsALine =
    msgTest "NewLine adds a line"
        app
        newLine
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            Array.length finalModel.lines
                |> Expect.equal (Array.length modelBeforeMsg.lines + 1)


splitsALineIntoTwo : Test
splitsALineIntoTwo =
    msgTest "NewLine splits a line into two"
        app
        newLine
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            let
                oldLine =
                    lineContent modelBeforeMsg.lines modelBeforeMsg.cursor.line
            in
            Expect.all
                [ \lines ->
                    lineContent lines (finalModel.cursor.line - 1)
                        |> Expect.equal (String.left modelBeforeMsg.cursor.column oldLine)
                , \lines ->
                    lineContent lines finalModel.cursor.line
                        |> Expect.equal (String.dropLeft modelBeforeMsg.cursor.column oldLine)
                ]
                finalModel.lines
