module Tests.RemoveCharAfter exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (isEndOfDocument, isLastColumn, isLastLine, lineContent, lineLength)
import Test exposing (..)
import Tests.Common exposing (..)


doesNothingOnEndOfDocument : Test
doesNothingOnEndOfDocument =
    msgTestWithPrecondition "RemoveCharAfter does nothing on start of document"
        app
        removeCharAfter
        (\model -> isEndOfDocument model.lines model.position)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel
                |> Expect.equal modelBeforeMsg


doesntMove : Test
doesntMove =
    msgTest "RemoveCharAfter doesn't move"
        app
        removeCharAfter
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.position
                |> Expect.equal modelBeforeMsg.position


decreasesLineCountOnLastColumnOfNotLastLine : Test
decreasesLineCountOnLastColumnOfNotLastLine =
    msgTestWithPrecondition "RemoveCharAfter decreases line count on last column of not last line"
        app
        removeCharAfter
        (\model ->
            isLastColumn model.lines model.position.line model.position.column
                && not (isLastLine model.lines model.position.line)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            Array.length finalModel.lines
                |> Expect.equal (Array.length modelBeforeMsg.lines - 1)


combinesLinesTogetherOnLastColumnOfNotLastLine : Test
combinesLinesTogetherOnLastColumnOfNotLastLine =
    msgTestWithPrecondition "RemoveCharAfter combines lines together on last column of not last line"
        app
        removeCharAfter
        (\model ->
            isLastColumn model.lines model.position.line model.position.column
                && not (isLastLine model.lines model.position.line)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            let
                oldLine1 =
                    lineContent modelBeforeMsg.lines modelBeforeMsg.position.line

                oldLine2 =
                    lineContent modelBeforeMsg.lines (modelBeforeMsg.position.line + 1)

                expectedNewLine =
                    oldLine1 ++ oldLine2

                newLine =
                    lineContent finalModel.lines finalModel.position.line
            in
            newLine
                |> Expect.equal expectedNewLine


decreasesCurrentLineLengthOnNotLastColumn : Test
decreasesCurrentLineLengthOnNotLastColumn =
    msgTestWithPrecondition "RemoveCharAfter decreases current line length on not last column"
        app
        removeCharAfter
        (\model -> not (isLastColumn model.lines model.position.line model.position.column))
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            let
                newLineLength =
                    lineLength finalModel.lines finalModel.position.line

                oldLineLength =
                    lineLength modelBeforeMsg.lines modelBeforeMsg.position.line
            in
            newLineLength
                |> Expect.equal (oldLineLength - 1)


removesTheCharOnNotLastColumn : Test
removesTheCharOnNotLastColumn =
    msgTestWithPrecondition "RemoveCharAfter removes the char on not last column"
        app
        removeCharAfter
        (\model -> not (isLastColumn model.lines model.position.line model.position.column))
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            let
                oldLine =
                    lineContent modelBeforeMsg.lines modelBeforeMsg.position.line

                oldLineBefore =
                    oldLine
                        |> String.left modelBeforeMsg.position.column

                oldLineAfter =
                    oldLine
                        |> String.dropLeft (modelBeforeMsg.position.column + 1)

                expectedNewLine =
                    oldLineBefore ++ oldLineAfter

                newLine =
                    lineContent finalModel.lines finalModel.position.line
            in
            newLine
                |> Expect.equal expectedNewLine
