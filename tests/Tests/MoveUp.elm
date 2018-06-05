module Tests.MoveUp exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (lineLength, startOfDocument)
import Test exposing (..)
import Tests.Common exposing (..)


jumpsToStartOfLineIfOnFirstLine : Test
jumpsToStartOfLineIfOnFirstLine =
    msgTestWithPrecondition "MoveUp jumps to start of line if on first line"
        app
        moveUp
        (\model -> model.cursor.line == 0)
    <|
        \_ _ _ _ finalModel ->
            finalModel.cursor
                |> Expect.equal startOfDocument


movesUpALineIfNotOnFirstLine : Test
movesUpALineIfNotOnFirstLine =
    msgTestWithPrecondition "MoveUp moves up a line if not on first line"
        app
        moveUp
        (\model -> model.cursor.line /= 0)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.line
                |> Expect.equal (modelBeforeMsg.cursor.line - 1)


staysOnSameColumnIfNotOnFirstLineAndEnoughCharsAboveCursor : Test
staysOnSameColumnIfNotOnFirstLineAndEnoughCharsAboveCursor =
    msgTestWithPrecondition "MoveUp stays on same column if not on first line and enough chars above cursor"
        app
        moveUp
        (\model ->
            (model.cursor.line /= 0)
                && (lineLength model.lines (model.cursor.line - 1) >= model.cursor.column)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.column
                |> Expect.equal modelBeforeMsg.cursor.column


movesToLastColumnIfNotOnFirstLineAndNoCharAboveCursor : Test
movesToLastColumnIfNotOnFirstLineAndNoCharAboveCursor =
    msgTestWithPrecondition "MoveUp moves to last column if not on first line and no char above cursor"
        app
        moveUp
        (\model ->
            (model.cursor.line /= 0)
                && (lineLength model.lines (model.cursor.line - 1) < model.cursor.column)
        )
    <|
        \_ _ _ _ finalModel ->
            finalModel.cursor.column
                |> Expect.equal
                    (lineLength
                        finalModel.lines
                        finalModel.cursor.line
                    )
