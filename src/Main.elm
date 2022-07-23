port module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, div, h4, input, label, node, option, p, section, select, span, text, textarea)
import Html.Attributes exposing (checked, class, disabled, min, placeholder, readonly, style, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Lazy
import Json.Encode as Encode
import Parser exposing (..)

-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


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


type alias Style =
    { color_scheme : String
    , font_face : String
    , font_point : String
    , horizontal : Bool
    , fullscreen : Bool
    , inline_preedit : Bool
    , preedit_type : String
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


type alias Model =
    { style : Style
    , color_scheme : ColorScheme
    , layout : Layout
    , yaml : String
    }


init : () -> ( Model, Cmd msg )
init _ =
    let
        model =
            { style =
                { color_scheme = "dracula"
                , font_face = "Microsoft YaHei"
                , font_point = "12"
                , horizontal = False
                , fullscreen = False
                , inline_preedit = False
                , preedit_type = "composition"
                , display_tray_icon = False
                , label_format = "%s."
                }
            , layout =
                { min_width = "160"
                , min_height = "0"
                , border_width = "3"
                , margin_x = "12"
                , margin_y = "12"
                , spacing = "10"
                , candidate_spacing = "5"
                , hilite_spacing = "4"
                , hilite_padding = "2"
                , round_corner = "4"
                }
            , color_scheme =
                { id = "dracula"
                , name = "Dracula"
                , author = "owlzou"
                , text_color = "#bd93f9"
                , candidate_text_color = "#f8f8f2"
                , comment_text_color = "#6272a4"
                , label_color = "#ffb86c"
                , back_color = "#282a36"
                , border_color = "#282a36"
                , hilited_text_color = "#f8f8f2"
                , hilited_back_color = "#44475a"
                , hilited_comment_text_color = "#8be9fd"
                , hilited_label_color = "#f1fa8c"
                , hilited_candidate_text_color = "#ff79c6"
                , hilited_candidate_back_color = "#44475a"
                }
            , yaml = ""
            }
    in
    ( model, setYAMLSource (themeEncoder { style = model.style, color_scheme = model.color_scheme, layout = model.layout }) )



-- UPDATE


type Msg
    = YAMLRecv String
    | UpdateStyle Style
    | UpdateLayout Layout
    | UpdateColorScheme ColorScheme
    | UpdateID String
    | NoOp


updateYAML : Model -> ( Model, Cmd msg )
updateYAML model =
    ( model, setYAMLSource (themeEncoder { style = model.style, color_scheme = model.color_scheme, layout = model.layout }) )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        YAMLRecv yamlString ->
            ( { model | yaml = yamlString }, setUI model.style.horizontal )

        UpdateStyle style ->
            updateYAML { model | style = style }

        UpdateLayout layout ->
            updateYAML { model | layout = layout }

        UpdateColorScheme color ->
            updateYAML { model | color_scheme = color }

        UpdateID id ->
            let
                style =
                    model.style

                colorScheme =
                    model.color_scheme

                newStyle =
                    { style | color_scheme = id }

                newColorScheme =
                    { colorScheme | id = id }
            in
            updateYAML { model | style = newStyle, color_scheme = newColorScheme }

        NoOp ->
            ( model, Cmd.none )



-- VIEW


fallbackColor : String -> String -> Html.Attribute msg
fallbackColor color fbk =
    style "color"
        (if String.isEmpty color then
            fbk

         else
            color
        )


candidateSpace : Bool -> String
candidateSpace horizontal =
    if horizontal then
        "padding-left"

    else
        "padding-bottom"


previewCandidateFirst : Model -> String -> String -> String -> Html Msg
previewCandidateFirst model label txt comment =
    div
        [ fallbackColor model.color_scheme.hilited_candidate_text_color model.color_scheme.candidate_text_color
        ]
        [ div
            [ style "background" model.color_scheme.hilited_candidate_back_color
            , style "border-radius" (model.layout.round_corner ++ "px")
            , style "padding" (model.layout.hilite_padding ++ "px")
            , class "candidate-bg"
            ]
            []
        , span
            [ style "color" model.color_scheme.hilited_label_color ]
            [ text label ]
        , span
            [ style "margin-right" (model.layout.hilite_spacing ++ "px")
            , style "margin-left" (model.layout.hilite_spacing ++ "px")
            ]
            [ text txt ]
        , span [ fallbackColor model.color_scheme.hilited_comment_text_color model.color_scheme.comment_text_color ] [ text comment ]
        ]


previewCandidate : Model -> String -> String -> String -> Html Msg
previewCandidate model label txt comment =
    div
        [ style "color" model.color_scheme.candidate_text_color
        , style (candidateSpace model.style.horizontal) (model.layout.candidate_spacing ++ "px")
        ]
        [ span [ style "color" model.color_scheme.label_color ] [ text label ]
        , span
            [ style "margin-right" (model.layout.hilite_spacing ++ "px")
            , style "margin-left" (model.layout.hilite_spacing ++ "px")
            ]
            [ text txt ]
        , span [ style "color" model.color_scheme.comment_text_color ] [ text comment ]
        ]


previewCandidateLast : Model -> String -> String -> String -> Html Msg
previewCandidateLast model label txt comment =
    div
        [ style "color" model.color_scheme.candidate_text_color, style (candidateSpace model.style.horizontal) (model.layout.candidate_spacing ++ "px") ]
        [ span [ style "color" model.color_scheme.label_color ] [ text label ]
        , span
            [ style "margin-right" (model.layout.hilite_spacing ++ "px")
            , style "margin-left" (model.layout.hilite_spacing ++ "px")
            ]
            [ text txt ]
        , span [ style "color" model.color_scheme.comment_text_color ] [ text comment ]
        ]


previewText : Model -> Html Msg
previewText model =
    div [ style "padding" (model.layout.margin_y ++ "px " ++ model.layout.margin_x ++ "px") ]
        [ -- 输入区域
          div
            [ style "margin-bottom" (model.layout.spacing ++ "px")
            , style "display"
                (if model.style.inline_preedit then
                    "none"

                 else
                    "block"
                )
            ]
            [ span
                [ style "color" model.color_scheme.text_color
                , style "display"
                    (if model.style.preedit_type == "composition" then
                        "inline"

                     else
                        "none"
                    )
                ]
                [ text "小狼毫" ]
            , span
                [ style "color" model.color_scheme.hilited_text_color
                , style "background-color" model.color_scheme.hilited_back_color
                , style "border-radius" (model.layout.round_corner ++ "px")
                , style "margin-right" (model.layout.hilite_spacing ++ "px")
                , style "margin-left" (model.layout.hilite_spacing ++ "px")
                , style "padding" (model.layout.hilite_padding ++ "px")
                ]
                [ text
                    (if model.style.preedit_type == "composition" then
                        "pei se"

                     else
                        "小狼毫配色"
                    )
                ]
            , span
                [ style "color" model.color_scheme.text_color ]
                [ text "ˆ" ]
            ]

        -- 待选项
        , div
            [ style "display"
                (if model.style.horizontal then
                    "flex"

                 else
                    "block"
                )
            , class "candidate"
            ]
            [ previewCandidateFirst model "1." "配色" "(pei se)"
            , previewCandidate model "2." "陪" "(pei)"
            , previewCandidate model "3." "配" "(pei)"
            , previewCandidate model "4." "赔" "(pei)"
            , previewCandidateLast model "5." "培" "(pei)"
            ]
        ]


preview : Model -> Html Msg
preview model =
    div []
        [ h4 [ class "title is-4" ] [ text "预览" ]
        , -- 内嵌输入
          div
            [ style "display"
                (if model.style.inline_preedit then
                    "block"

                 else
                    "none"
                )
            , style "margin-bottom" "8px"
            ]
            [ span [ style "border-bottom" "1px dashed #000" ]
                [ text
                    (if model.style.preedit_type == "composition" then
                        "小狼毫pei se"

                     else
                        "小狼毫配色"
                    )
                ]
            ]
        , div
            [ class "rime"
            , style "position" "relative"
            , style "min-width" (model.layout.min_width ++ "px")
            , style "min-height" (model.layout.min_height ++ "px")
            ]
            [ -- 背景
              div
                [ class "bg"
                , style "background-color" model.color_scheme.back_color
                ]
                []

            -- 模拟边框
            , div
                [ class "border"
                , style "border-style" "solid"
                , style "border-width" (model.layout.border_width ++ "px")
                , style "border-color" model.color_scheme.border_color
                ]
                []

            -- 预览界面
            , div
                [ class "text"
                , style "width" "fit-content"
                , style "font-family" ("\"" ++ model.style.font_face ++ "\"")
                , style "font-size" (String.fromFloat (Maybe.withDefault 12.0 (String.toFloat model.style.font_point) / 12.0) ++ "rem")
                ]
                [ previewText model ]
            ]
        ]


colorSchemeInputView : ColorScheme -> Html Msg
colorSchemeInputView model =
    div []
        [ h4 [ class "title is-4" ] [ text "配色方案" ]
        , textInput "代码" "ID" (Just "小写英文，无空格，非数字开头。") model.id (\i -> UpdateID i)
        , textInput "名字" "name" Nothing model.name (\i -> UpdateColorScheme { model | name = i })
        , textInput "作者" "author" Nothing model.author (\i -> UpdateColorScheme { model | author = i })
        , divider "背景设定"
        , colorInput "背景颜色" "back_color" model.back_color (\i -> UpdateColorScheme { model | back_color = i })
        , colorInput "边框颜色" "border_color" model.border_color (\i -> UpdateColorScheme { model | border_color = i })
        , divider "内选区域"
        , colorInput "文字颜色" "text_color" model.text_color (\i -> UpdateColorScheme { model | text_color = i })
        , colorInput "编码颜色" "hilited_text_color" model.hilited_text_color (\i -> UpdateColorScheme { model | hilited_text_color = i })
        , colorInput "背景颜色" "hilited_back_color" model.hilited_back_color (\i -> UpdateColorScheme { model | hilited_back_color = i })
        , divider "激活候选项"
        , colorInput "文字颜色" "hilited_candidate_text_color" model.hilited_candidate_text_color (\i -> UpdateColorScheme { model | hilited_candidate_text_color = i })
        , colorInput "提示颜色" "hilited_comment_text_color" model.hilited_comment_text_color (\i -> UpdateColorScheme { model | hilited_comment_text_color = i })
        , colorInput "标签颜色" "hilited_label_color" model.hilited_label_color (\i -> UpdateColorScheme { model | hilited_label_color = i })
        , colorInput "背景颜色" "hilited_candidate_back_color" model.hilited_candidate_back_color (\i -> UpdateColorScheme { model | hilited_candidate_back_color = i })
        , divider "其他候选项"
        , colorInput "文字颜色" "candidate_text_color" model.candidate_text_color (\i -> UpdateColorScheme { model | candidate_text_color = i })
        , colorInput "提示颜色" "comment_text_color" model.comment_text_color (\i -> UpdateColorScheme { model | comment_text_color = i })
        , colorInput "标签颜色" "label_color" model.label_color (\i -> UpdateColorScheme { model | label_color = i })
        ]


styleInputView : Style -> Html Msg
styleInputView style =
    div []
        [ h4 [ class "title is-4" ] [ text "风格" ]
        , textInputDisabled "当前配色方案" "color_scheme" style.color_scheme
        , textInput "字体" "font_face" Nothing style.font_face (\i -> UpdateStyle { style | font_face = i })
        , numberInput "字号" "font_point" (Just "大小完全不一样，预览仅供参考。") style.font_point (\i -> UpdateStyle { style | font_point = i })
        , boolInput "横排候选" "horizontal" Nothing style.horizontal (\i -> UpdateStyle { style | horizontal = i })
        , boolInput "全屏" "fullscreen" (Just "字面意义上的全屏，无法展示。") style.fullscreen (\i -> UpdateStyle { style | fullscreen = i })
        , boolInput "内嵌编码" "inline_preedit" Nothing style.inline_preedit (\i -> UpdateStyle { style | inline_preedit = i })
        , div [ class "field", class "is-horizontal" ]
            [ label [ class "label", class "field-label", title "preedit_type" ] [ text "预输入类型" ]
            , div [ class "field-body" ]
                [ div [ class "control" ]
                    [ div [ class "select" ]
                        [ select [ value style.preedit_type ]
                            [ option [ value "composition", onClick (UpdateStyle { style | preedit_type = "composition" }) ] [ text "composition / 组合" ]
                            , option [ value "preview", onClick (UpdateStyle { style | preedit_type = "preview" }) ] [ text "preview / 预览" ]
                            ]
                        ]
                    ]
                ]
            ]
        , boolInput "显示托盘图标" "display_tray_icon" Nothing style.display_tray_icon (\i -> UpdateStyle { style | display_tray_icon = i })
        , textInput "标签格式" "label_format" (Just "%s 代表序号") style.label_format (\i -> UpdateStyle { style | label_format = i })
        ]


layoutInputView : Layout -> Html Msg
layoutInputView layout =
    div []
        [ divider "布局"
        , numberInput "最小宽度" "min_width" Nothing layout.min_width (\i -> UpdateLayout { layout | min_width = i })
        , numberInput "最小高度" "min_height" Nothing layout.min_height (\i -> UpdateLayout { layout | min_height = i })
        , numberInput "边框宽度" "border_width" Nothing layout.border_width (\i -> UpdateLayout { layout | border_width = i })
        , numberInput "横向外边距" "margin_x" Nothing layout.margin_x (\i -> UpdateLayout { layout | margin_x = i })
        , numberInput "纵向外边距" "margin_y" Nothing layout.margin_y (\i -> UpdateLayout { layout | margin_y = i })
        , numberInput "输入和候选间距" "spacing" Nothing layout.spacing (\i -> UpdateLayout { layout | spacing = i })
        , numberInput "候选项间距" "candidate_spacing" Nothing layout.candidate_spacing (\i -> UpdateLayout { layout | candidate_spacing = i })
        , numberInput "高亮字间距" "hilite_spacing" Nothing layout.hilite_spacing (\i -> UpdateLayout { layout | hilite_spacing = i })
        , numberInput "高亮背景内边距" "hilite_padding" Nothing layout.hilite_padding (\i -> UpdateLayout { layout | hilite_padding = i })
        , numberInput "高亮背景圆角" "round_corner" Nothing layout.round_corner (\i -> UpdateLayout { layout | round_corner = i })
        ]


yamlOutput : Model -> Html Msg
yamlOutput model =
    div [ class "field" ]
        [ h4 [ class "title is-4" ] [ text "输出" ]
        , div [ class "control" ]
            [ textarea
                [ class "textarea"
                , style "font-family" "consolas"
                , readonly True
                , value model.yaml
                ]
                []
            ]
        ]


hero : Html Msg
hero =
    section [ class "hero is-info" ]
        [ div [ class "hero-body" ]
            [ p [ class "title" ] [ text "小狼毫皮肤编辑器" ]
            , p [ class "subtitle" ] [ text "预览仅供参考 (～￣▽￣)～" ]
            ]
        ]


view : Model -> Document Msg
view model =
    { title = "小狼毫皮肤编辑器"
    , body =
        [ hero
        , node "section"
            [ class "section" ]
            [ div [ class "tile is-ancestor", class "rime_options" ]
                [ div [ class "tile is-parent is-vertical" ]
                    [ div [ class "fakepreview" ] []
                    , div [ class "tile is-child box preview", style "flex-grow" "0" ] [ preview model ]
                    , div [ class "tile is-child box yaml" ] [ yamlOutput model ]
                    ]
                , div [ class "tile is-parent" ] [ div [ class "tile is-child box" ] [ Html.Lazy.lazy colorSchemeInputView model.color_scheme ] ]
                , div [ class "tile is-parent" ] [ div [ class "tile is-child box" ] [ Html.Lazy.lazy styleInputView model.style, Html.Lazy.lazy layoutInputView model.layout ] ]
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions _ =
    yamlReceiver YAMLRecv



-- PORTS


port setYAMLSource : Encode.Value -> Cmd msg


port yamlReceiver : (String -> msg) -> Sub msg



-- 更新UI


port setUI : Bool -> Cmd msg



-- UI


helpUI : Maybe String -> Html msg
helpUI help =
    case help of
        Just h ->
            p [ class "help" ] [ text h ]

        Nothing ->
            span [] []


textInput : String -> String -> Maybe String -> String -> (String -> Msg) -> Html Msg
textInput label_ id_ help val updateFunc =
    div [ class "field", class "is-horizontal" ]
        [ label [ class "label", class "field-label", title id_ ] [ text label_ ]
        , div [ class "field-body" ]
            [ div [ class "control" ]
                [ input [ class "input", value val, placeholder id_, onInput updateFunc ] []
                , helpUI help
                ]
            ]
        ]


textInputDisabled : String -> String -> String -> Html Msg
textInputDisabled name tip val =
    div [ class "field", class "is-horizontal" ]
        [ label [ class "label", class "field-label", title tip ] [ text name ]
        , div [ class "field-body" ] [ div [ class "control" ] [ input [ class "input", value val, placeholder tip, disabled True ] [] ] ]
        ]


colorInput : String -> String -> String -> (String -> Msg) -> Html Msg
colorInput name tip val updateFunc =
    div [ class "field", class "is-horizontal" ]
        [ label [ class "label", class "field-label", title tip ] [ text name ]
        , div [ class "field-body" ]
            [ div [ class "field", class "is-grouped" ]
                [ div [ class "control" ] [ input [ class "input", value val, placeholder tip, onInput updateFunc ] [] ]
                , div [ class "control" ] [ input [ type_ "color", value val, onInput updateFunc ] [] ]
                ]
            ]
        ]


boolInput : String -> String -> Maybe String -> Bool -> (Bool -> Msg) -> Html Msg
boolInput name tip help val updateFunc =
    div [ class "field", class "is-horizontal" ]
        [ label [ class "field-label", class "label", title tip ] [ text name ]
        , div [ class "field-body" ]
            [ div [ class "control" ]
                [ label [ class "checkbox" ]
                    [ input [ type_ "checkbox", checked val, onCheck updateFunc ] [] ]
                , helpUI help
                ]
            ]
        ]


numberInput : String -> String -> Maybe String -> String -> (String -> Msg) -> Html Msg
numberInput name tip help val updateFunc =
    div [ class "field", class "is-horizontal" ]
        [ label [ class "field-label", class "label", title tip ] [ text name ]
        , div [ class "field-body" ]
            [ div [ class "control" ]
                [ input [ type_ "number", class "input", value val, Html.Attributes.min "0", onInput updateFunc ] []
                , helpUI help
                ]
            ]
        ]


divider : String -> Html msg
divider txt =
    div [ class "divider is-left" ] [ text txt ]



-- COLOR HELPER


type alias Color =
    { red : String
    , green : String
    , blue : String
    }


hexToColor : Parser Color
hexToColor =
    let
        get2Char : Parser String
        get2Char =
            getChompedString <|
                chompIf Char.isHexDigit
                    |. chompIf Char.isHexDigit
    in
    succeed Color
        |. symbol "#"
        |= get2Char
        |= get2Char
        |= get2Char
        |. end


colorToBGR : Color -> String
colorToBGR color =
    "0x" ++ color.blue ++ color.green ++ color.red



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
        , ( "preedit_type", Encode.string style.preedit_type )
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
    case run hexToColor str of
        Ok val ->
            Encode.string (colorToBGR val)

        Err _ ->
            Encode.string ""
