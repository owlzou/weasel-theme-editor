module Model exposing (..)

import ColorHelper exposing (hex2BGR)
import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing (..)


type alias Theme =
    { style : Style
    , color_scheme : ColorScheme
    , layout : Layout
    }


type alias ColorScheme =
    { id : String
    , name : String
    , author : String

    -- 背景设定
    , back_color : String
    , border_color : String

    -- 内选区域
    , text_color : String
    , hilited_text_color : String
    , hilited_back_color : String

    -- 激活候选项
    , hilited_comment_text_color : String
    , hilited_candidate_text_color : String
    , hilited_candidate_back_color : String
    , hilited_label_color : String

    -- 其他候选项
    , candidate_text_color : String
    , comment_text_color : String
    , label_color : String
    }


type PreEditType
    = Composition
    | Preview


preEditTypeToString : PreEditType -> String
preEditTypeToString pre =
    case pre of
        Composition ->
            "composition"

        Preview ->
            "preview"


type alias Style =
    { color_scheme : String
    , font_face : String
    , font_point : String
    , horizontal : Bool
    , fullscreen : Bool
    , inline_preedit : Bool
    , preedit_type : PreEditType
    , display_tray_icon : Bool
    , label_format : String
    }


type alias Layout =
    { min_width : String
    , min_height : String
    , border_width : String
    , margin_x : String
    , margin_y : String
    , spacing : String
    , candidate_spacing : String
    , hilite_spacing : String
    , hilite_padding : String
    , round_corner : String
    }


type alias TextSize =
    { width : Float
    , height : Float
    , fullCommentWidth : Float
    , commentWidth : Float
    , labelWidth : Float
    }


type alias Model =
    { style : Style
    , color_scheme : ColorScheme
    , layout : Layout
    , yaml : String
    , textSize : TextSize
    }


initModel : Model
initModel =
    { style =
        { color_scheme = "succulent_plant"
        , font_face = "Microsoft YaHei"
        , font_point = "12"
        , horizontal = True
        , fullscreen = False
        , inline_preedit = True
        , preedit_type = Composition
        , display_tray_icon = False
        , label_format = "%s."
        }
    , layout =
        { min_width = "100"
        , min_height = "0"
        , border_width = "3"
        , margin_x = "10"
        , margin_y = "10"
        , spacing = "10"
        , candidate_spacing = "20"
        , hilite_spacing = "5"
        , hilite_padding = "10"
        , round_corner = "0"
        }
    , color_scheme =
        { id = "succulent_plant"
        , name = "多肉植物"
        , author = "owlzou"
        , text_color = "#7e643c"
        , candidate_text_color = "#7e643c"
        , comment_text_color = "#547979"
        , label_color = ""
        , back_color = "#e6e7e9"
        , border_color = "#e6e7e9"
        , hilited_text_color = "#7e643c"
        , hilited_back_color = "#547979"
        , hilited_comment_text_color = "#a6ca82"
        , hilited_label_color = ""
        , hilited_candidate_text_color = "#e6e7e9"
        , hilited_candidate_back_color = "#547979"
        }
    , yaml = ""
    , textSize =
        { width = 12
        , height = 12
        , fullCommentWidth = 12
        , commentWidth = 6
        , labelWidth = 24
        }
    }



-- ENCODER


themeEncoder : Theme -> Encode.Value
themeEncoder model =
    Encode.object
        [ ( "style", styleEncoder model.style )
        , ( "preset_color_schemes/" ++ model.color_scheme.id, colorSchemeEncoder model.color_scheme )
        , ( "layout", layoutEncoder model.layout )
        ]


styleEncoder : Style -> Encode.Value
styleEncoder style =
    Encode.object
        [ ( "color_scheme", Encode.string style.color_scheme )
        , ( "font_face", Encode.string style.font_face )
        , ( "font_point", str2IntEncoder style.font_point )
        , ( "horizontal", Encode.bool style.horizontal )
        , ( "fullscreen", Encode.bool style.fullscreen )
        , ( "inline_preedit", Encode.bool style.inline_preedit )
        , ( "preedit_type", Encode.string (preEditTypeToString style.preedit_type) )
        , ( "display_tray_icon", Encode.bool style.display_tray_icon )
        , ( "label_format", Encode.string style.label_format )
        ]


layoutEncoder : Layout -> Encode.Value
layoutEncoder layout =
    Encode.object
        [ ( "min_width", str2IntEncoder layout.min_width )
        , ( "min_height", str2IntEncoder layout.min_height )
        , ( "border_width", str2IntEncoder layout.border_width )
        , ( "margin_x", str2IntEncoder layout.margin_x )
        , ( "margin_y", str2IntEncoder layout.margin_y )
        , ( "spacing", str2IntEncoder layout.spacing )
        , ( "candidate_spacing", str2IntEncoder layout.candidate_spacing )
        , ( "hilite_spacing", str2IntEncoder layout.hilite_spacing )
        , ( "hilite_padding", str2IntEncoder layout.hilite_padding )
        , ( "round_corner", str2IntEncoder layout.round_corner )
        ]


colorSchemeEncoder : ColorScheme -> Encode.Value
colorSchemeEncoder color =
    Encode.object
        [ ( "name", Encode.string color.name )
        , ( "author", Encode.string color.author )
        , ( "back_color", bgrEncoder color.back_color )
        , ( "border_color", bgrEncoder color.border_color )
        , ( "text_color", bgrEncoder color.text_color )
        , ( "hilited_text_color", bgrEncoder color.hilited_text_color )
        , ( "hilited_back_color", bgrEncoder color.hilited_back_color )
        , ( "hilited_comment_text_color", bgrEncoder color.hilited_comment_text_color )
        , ( "hilited_candidate_text_color", bgrEncoder color.hilited_candidate_text_color )
        , ( "hilited_candidate_back_color", bgrEncoder color.hilited_candidate_back_color )
        , ( "hilited_label_color", bgrEncoder color.hilited_label_color )
        , ( "candidate_text_color", bgrEncoder color.candidate_text_color )
        , ( "comment_text_color", bgrEncoder color.comment_text_color )
        , ( "label_color", bgrEncoder color.label_color )
        ]



-- ENCODE HELPER


str2IntEncoder : String -> Encode.Value
str2IntEncoder str =
    Encode.int (Maybe.withDefault 0 (String.toInt str))


bgrEncoder : String -> Encode.Value
bgrEncoder str =
    Encode.string (hex2BGR str)



-- TEXT


textSizeDecoder : Decode.Decoder TextSize
textSizeDecoder =
    Decode.map5 TextSize
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)
        (Decode.field "fullCommentWidth" Decode.float)
        (Decode.field "commentWidth" Decode.float)
        (Decode.field "labelWidth" Decode.float)


decodeTextSize : Encode.Value -> TextSize
decodeTextSize value =
    case Decode.decodeValue textSizeDecoder value of
        Ok val ->
            val

        Err _ ->
            initModel.textSize



-- LABEL


type LabelType
    = Text String
    | Index


textParser : Parser LabelType
textParser =
    succeed Text
        |= (getChompedString <| chompIf (\_ -> True))


idxParser : Parser LabelType
idxParser =
    succeed Index
        |. symbol "%"
        |. symbol "s"


stepParser : List LabelType -> Parser (Step (List LabelType) (List LabelType))
stepParser cmds =
    let
        mergeText : LabelType -> Step (List LabelType) (List LabelType)
        mergeText next =
            case next of
                Text str ->
                    case cmds of
                        x :: xs ->
                            case x of
                                Text baseStr ->
                                    Parser.Loop (Text (baseStr ++ str) :: xs)

                                _ ->
                                    Parser.Loop (next :: cmds)

                        _ ->
                            Parser.Loop (next :: cmds)

                _ ->
                    Parser.Loop (next :: cmds)

        nextParser : Parser LabelType -> Parser (Step (List LabelType) (List LabelType))
        nextParser parser =
            succeed mergeText
                |= parser

        loopList : List (Parser (Step (List LabelType) (List LabelType)))
        loopList =
            List.map nextParser [ Parser.backtrackable idxParser, textParser ]
    in
    oneOf <|
        List.append loopList
            [ succeed () |> Parser.map (\_ -> Parser.Done (List.reverse cmds)) ]


labelParser : Parser (List LabelType)
labelParser =
    Parser.loop [] stepParser


getLabel : Int -> String -> String
getLabel idx format =
    case run labelParser format of
        Ok lblList ->
            lblList
                |> List.map
                    (\i ->
                        case i of
                            Text t ->
                                t

                            Index ->
                                String.fromInt idx
                    )
                |> String.join ""

        Err _ ->
            ""
