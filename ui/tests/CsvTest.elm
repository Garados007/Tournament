module CsvTest exposing (suite)

import Csv
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Csv.encode"
        [ test "starts with a byte order mark and ends lines with CRLF" <|
            \_ ->
                Csv.encode [ [ "a", "b" ], [ "c", "d" ] ]
                    |> Expect.equal "\u{FEFF}a;b\u{000D}\nc;d\u{000D}\n"
        , test "fields with a separator, quote or line break are quoted" <|
            \_ ->
                Csv.encode [ [ "a;b", "say \"hi\"", "x\ny", "plain" ] ]
                    |> Expect.equal "\u{FEFF}\"a;b\";\"say \"\"hi\"\"\";\"x\ny\";plain\u{000D}\n"
        , test "empty fields stay empty" <|
            \_ ->
                Csv.encode [ [ "", "", "" ] ]
                    |> Expect.equal "\u{FEFF};;\u{000D}\n"
        ]
