module Tests exposing (..)

import Expect
import Fuzz
import Random
import Test exposing (Test)


fromBools : Test
fromBools =
    Test.describe "Bits.fromBools"
        []
