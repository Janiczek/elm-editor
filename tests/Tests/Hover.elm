module Tests.Hover exposing (..)

import ArchitectureTest exposing (..)
import Expect exposing (Expectation)
import Main exposing (..)
import Test exposing (..)
import Tests.Common exposing (..)


setsHoverWithinBounds : Test
setsHoverWithinBounds =
    msgTest "Hover sets hover within bounds" app hover <|
        \_ _ modelBeforeMsg msg finalModel ->
            case msg of
                Hover hover ->
                    case hover of
                        NoHover ->
                            finalModel.hover
                                |> Expect.equal NoHover

                        HoverLine _ ->
                            case finalModel.hover of
                                NoHover ->
                                    Expect.fail "hover should have been HoverLine"

                                HoverLine line ->
                                    line
                                        |> Expect.all
                                            [ Expect.atLeast 0
                                            , Expect.atMost (lastLine modelBeforeMsg.lines)
                                            ]

                                HoverChar _ ->
                                    Expect.fail "hover should have been HoverLine"

                        HoverChar _ ->
                            case finalModel.hover of
                                NoHover ->
                                    Expect.fail "hover should have been HoverChar"

                                HoverLine _ ->
                                    Expect.fail "hover should have been HoverChar"

                                HoverChar position ->
                                    position
                                        |> Expect.all
                                            [ .line >> Expect.atLeast 0
                                            , .line >> Expect.atMost (lastLine modelBeforeMsg.lines)
                                            , .column >> Expect.atLeast 0
                                            , \{ line, column } ->
                                                column
                                                    |> Expect.atMost (lastColumn modelBeforeMsg.lines line)
                                            ]

                _ ->
                    Expect.pass
