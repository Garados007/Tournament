module Csv exposing (encode)

{-| CSV as spreadsheet programs with European settings expect it: `;` as the
separator, UTF-8 with a byte order mark and CRLF line ends.
-}


encode : List (List String) -> String
encode rows =
    "\u{FEFF}"
        ++ (rows
                |> List.map (List.map field >> String.join ";")
                |> List.map (\row -> row ++ "\u{000D}\n")
                |> String.concat
           )


field : String -> String
field value =
    if String.any (\char -> char == ';' || char == '"' || char == '\n' || char == '\u{000D}') value then
        "\"" ++ String.replace "\"" "\"\"" value ++ "\""

    else
        value
