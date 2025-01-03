module ColorHelper exposing (blendColor, hex2BGR, hex2ColorFallback, hex2ColorWithDefault, transparentColor)

import Color
import Parser exposing (..)


type alias ColorStr =
    { red : String
    , green : String
    , blue : String
    }


hexToColorStr : Parser ColorStr
hexToColorStr =
    let
        hexToInt : Parser String
        hexToInt =
            getChompedString <|
                chompIf Char.isHexDigit
                    |. chompIf Char.isHexDigit
    in
    succeed ColorStr
        |. symbol "#"
        |= hexToInt
        |= hexToInt
        |= hexToInt
        |. end


intFromHexString : String -> Int
intFromHexString hex =
    let
        singleHex c =
            if Char.isDigit c then
                Char.toCode c - Char.toCode '0'

            else
                Char.toCode c - Char.toCode 'A' + 10

        power i =
            List.product (List.repeat i 16)
    in
    String.toList (String.toUpper hex)
        |> List.indexedMap (\i item -> power (String.length hex - i - 1) * singleHex item)
        |> List.sum


hex2BGR : String -> String
hex2BGR hex =
    case run hexToColorStr hex of
        Ok color ->
            "0x" ++ color.blue ++ color.green ++ color.red

        Err _ ->
            ""


hex2ColorWithDefault : Color.Color -> String -> Color.Color
hex2ColorWithDefault default hex =
    case run hexToColorStr hex of
        Ok color ->
            Color.rgb255 (intFromHexString color.red) (intFromHexString color.green) (intFromHexString color.blue)

        Err _ ->
            default


hex2ColorFallback : Color.Color -> String -> String -> Color.Color
hex2ColorFallback default hex2 hex1 =
    case run hexToColorStr hex1 of
        Ok color ->
            Color.rgb255 (intFromHexString color.red) (intFromHexString color.green) (intFromHexString color.blue)

        Err _ ->
            case run hexToColorStr hex2 of
                Ok color ->
                    Color.rgb255 (intFromHexString color.red) (intFromHexString color.green) (intFromHexString color.blue)

                Err _ ->
                    default


blendColor : Color.Color -> Color.Color -> Color.Color
blendColor colorA colorB =
    let
        a =
            Color.toRgba colorA

        b =
            Color.toRgba colorB
    in
    Color.rgb ((a.red * 2 + b.red) / 3) ((a.green * 2 + b.green) / 3) ((a.blue * 2 + b.blue) / 3)


transparentColor : Color.Color
transparentColor =
    Color.rgba 0.0 0.0 0.0 0.0
