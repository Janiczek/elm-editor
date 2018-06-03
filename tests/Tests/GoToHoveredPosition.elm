module Tests.GoToHoveredPosition exposing (..)

import ArchitectureTest exposing (..)
import Expect exposing (Expectation)
import Main exposing (..)
import Test exposing (..)
import Tests.Common exposing (..)


doesNothingIfHoverIsNoHover : Test
doesNothingIfHoverIsNoHover =
    msgTestWithPrecondition "GoToHoveredPosition does nothing if hover is NoHover"
        app
        goToHoveredPosition
        (\model -> model.hover == NoHover)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            modelBeforeMsg
                |> Expect.equal finalModel


movesToLastColumnOfHoveredLineIfHoverIsHoverLine : Test
movesToLastColumnOfHoveredLineIfHoverIsHoverLine =
    msgTestWithPrecondition "GoToHoveredPosition moves to last column of hovered line if hover is HoverLine"
        app
        goToHoveredPosition
        (\model ->
            case model.hover of
                NoHover ->
                    False

                HoverLine _ ->
                    True

                HoverChar _ ->
                    False
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            case modelBeforeMsg.hover of
                NoHover ->
                    Expect.fail "hover should have been HoverLine"

                HoverLine line ->
                    finalModel.position
                        |> Expect.equal
                            { line = line
                            , column = lastColumn modelBeforeMsg.lines line
                            }

                HoverChar _ ->
                    Expect.fail "hover should have been HoverLine"


movesToHoveredPositionIfHoverIsHoverChar : Test
movesToHoveredPositionIfHoverIsHoverChar =
    msgTestWithPrecondition "GoToHoveredPosition moves to hovered position if hover is HoverChar"
        app
        goToHoveredPosition
        (\model ->
            case model.hover of
                NoHover ->
                    False

                HoverLine _ ->
                    False

                HoverChar _ ->
                    True
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            case modelBeforeMsg.hover of
                NoHover ->
                    Expect.fail "hover should have been HoverChar"

                HoverLine _ ->
                    Expect.fail "hover should have been HoverChar"

                HoverChar { line, column } ->
                    finalModel.position
                        |> Expect.equal
                            { line = line
                            , column = column
                            }
