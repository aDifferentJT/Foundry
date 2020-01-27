module Expect.Extra exposing (implies)

import Expect exposing (Expectation)


implies : Bool -> (() -> Expectation) -> Expectation
implies c e =
    if c then
        e ()

    else
        Expect.pass
