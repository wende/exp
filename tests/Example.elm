module Example exposing (suite)

import Exp exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)



-- dummy meta


wm : Domain -> WithMeta
wm d =
    { meta = { id = 0, line = 0 }, domain = d }


isError : Result DomainError e -> Bool
isError e =
    case e of
        Err (DomainError _ _) ->
            True

        _ ->
            False


suite : Test
suite =
    describe "Simple reduction"
        [ test "Reduces two Anys" <|
            \() -> Exp.solveDomains (wm Any) (wm Any) |> Expect.equal (Ok <| wm Any)
        , test "Reduces two list OR" <|
            \() ->
                Exp.solveDomains
                    (wm <| Or [ wm <| DInt <| ExactNumber 1, wm <| DInt <| ExactNumber 2 ])
                    (wm <| Or [ wm <| DInt <| ExactNumber 3 ])
                    |> Expect.equal (Ok <| wm <| Or [ wm <| DInt <| ExactNumber 1, wm <| DInt <| ExactNumber 2, wm <| DInt <| ExactNumber 3 ])
        , test "Reduces two list And" <|
            \() ->
                Exp.solveDomains
                    (wm <| And [ wm <| DInt <| ExactNumber 1, wm <| DInt <| ExactNumber 2 ])
                    (wm <| And [ wm <| DInt <| ExactNumber 3 ])
                    |> Expect.equal (Ok <| wm <| And [ wm <| DInt <| ExactNumber 1, wm <| DInt <| ExactNumber 2, wm <| DInt <| ExactNumber 3 ])
        , test "2 is not equal 3" <|
            \() ->
                Exp.solveDomains (wm <| DInt <| ExactNumber 2) (wm <| DInt <| ExactNumber 3)
                    |> isError
                    |> Expect.true "2 is not equal 3"
        ]
