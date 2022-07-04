module UtilitiesTest exposing (testRemoveFirst)

import Expect
import Test exposing (Test, describe, test)
import Utilities



--| Values


testRemoveFirst : Test
testRemoveFirst =
    let
        r =
            Utilities.removeFirst ((/=) 'b')

        remove1 =
            r [ 'a', 'b', 'b', 'c', 'b' ]

        remove2 =
            r remove1

        remove3 =
            r remove2

        countB =
            List.length << List.filter ((==) 'b')
    in
    describe "remove letter b"
        [ test "Should be twice left" <|
            \_ ->
                Expect.equal 2 (countB remove1)
        , test "Should be once left" <|
            \_ ->
                Expect.equal 1 (countB remove2)
        , test "Should be zero left" <|
            \_ ->
                Expect.equal 0 (countB remove3)
        ]
