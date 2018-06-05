module Tests.MoveDown exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (endOfDocument, lastColumn, lastLine, lineLength)
import Test exposing (..)
import Tests.Common exposing (..)


jumpsToEndOfLineIfOnLastLine : Test
jumpsToEndOfLineIfOnLastLine =
    msgTestWithPrecondition "MoveDown jumps to end of line if on last line"
        app
        moveDown
        (\model -> model.cursor.line == lastLine model.lines)
    <|
        \_ _ _ _ finalModel ->
            finalModel.cursor
                |> Expect.equal (endOfDocument finalModel.lines)


movesDownALineIfNotOnLastLine : Test
movesDownALineIfNotOnLastLine =
    msgTestWithPrecondition "MoveDown moves down a line if not on last line"
        app
        moveDown
        (\model -> model.cursor.line /= lastLine model.lines)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.line
                |> Expect.equal (modelBeforeMsg.cursor.line + 1)


staysOnSameColumnIfNotOnLastLineAndEnoughCharsBelowCursor : Test
staysOnSameColumnIfNotOnLastLineAndEnoughCharsBelowCursor =
    msgTestWithPrecondition "MoveDown stays on same column if not on last line and enough chars below cursor"
        app
        moveDown
        (\model ->
            (model.cursor.line /= lastLine model.lines)
                && (lineLength model.lines (model.cursor.line + 1) >= model.cursor.column)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.column
                |> Expect.equal modelBeforeMsg.cursor.column


movesToLastColumnIfNotOnLastLineAndNoCharBelowCursor : Test
movesToLastColumnIfNotOnLastLineAndNoCharBelowCursor =
    msgTestWithPrecondition "MoveDown moves to last column if not on last line and no char below cursor"
        app
        moveDown
        (\model ->
            (model.cursor.line /= lastLine model.lines)
                && (lineLength model.lines (model.cursor.line + 1) < model.cursor.column)
        )
    <|
        \_ _ _ _ finalModel ->
            finalModel.cursor.column
                |> Expect.equal
                    (lastColumn
                        finalModel.lines
                        finalModel.cursor.line
                    )
