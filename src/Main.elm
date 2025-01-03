port module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, a, code, div, footer, h4, input, label, li, option, p, section, select, span, text, textarea, ul)
import Html.Attributes exposing (checked, class, disabled, href, placeholder, readonly, style, target, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Lazy
import Json.Encode as Encode
import Model exposing (ColorScheme, Layout, Model, PreEditType(..), Style, decodeTextSize, initModel, preEditTypeToString, themeEncoder)
import Parser exposing (..)
import Render exposing (canvasView)



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


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel, setYAMLSource (themeEncoder { style = initModel.style, color_scheme = initModel.color_scheme, layout = initModel.layout }) )



-- UPDATE


type Msg
    = YAMLRecv String
    | SizeRecv Encode.Value
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
            ( { model | yaml = yamlString }, Cmd.none )

        SizeRecv value ->
            ( { model | textSize = decodeTextSize value }, Cmd.none )

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
                [ case model.style.preedit_type of
                    Composition ->
                        text "小狼毫pei se"

                    Preview ->
                        text "小狼毫配色"
                ]
            ]
        , canvasView model
        ]


colorSchemeInputView : ColorScheme -> Html Msg
colorSchemeInputView model =
    div []
        [ h4 [ class "title is-4" ] [ text "配色方案" ]
        , textInput "代码" "ID" (Just "小写英文，无空格，非数字开头。") model.id (\i -> UpdateID i)
        , textInput "名称" "name" Nothing model.name (\i -> UpdateColorScheme { model | name = i })
        , textInput "作者" "author" Nothing model.author (\i -> UpdateColorScheme { model | author = i })
        , divider "整体设定"
        , colorInput "文字颜色" "text_color" Nothing model.text_color (\i -> UpdateColorScheme { model | text_color = i })
        , colorInput "背景颜色" "back_color" Nothing model.back_color (\i -> UpdateColorScheme { model | back_color = i })
        , colorInput "边框颜色" "border_color" (Just "无边框效果需边框和背景为同一颜色。") model.border_color (\i -> UpdateColorScheme { model | border_color = i })
        , colorInput "阴影颜色" "shadow_color" Nothing model.shadow_color (\i -> UpdateColorScheme { model | shadow_color = i })
        , colorInput "标签颜色" "label_color" Nothing model.label_color (\i -> UpdateColorScheme { model | label_color = i })
        , colorInput "提示颜色" "comment_text_color" Nothing model.comment_text_color (\i -> UpdateColorScheme { model | comment_text_color = i })
        , divider "内选区域"
        , colorInput "编码颜色" "hilited_text_color" Nothing model.hilited_text_color (\i -> UpdateColorScheme { model | hilited_text_color = i })
        , colorInput "背景颜色" "hilited_back_color" Nothing model.hilited_back_color (\i -> UpdateColorScheme { model | hilited_back_color = i })
        , colorInput "阴影颜色" "hilited_shadow_color" Nothing model.hilited_shadow_color (\i -> UpdateColorScheme { model | hilited_shadow_color = i })
        , divider "激活候选项"
        , colorInput "文字颜色" "hilited_candidate_text_color" Nothing model.hilited_candidate_text_color (\i -> UpdateColorScheme { model | hilited_candidate_text_color = i })
        , colorInput "提示颜色" "hilited_comment_text_color" Nothing model.hilited_comment_text_color (\i -> UpdateColorScheme { model | hilited_comment_text_color = i })
        , colorInput "标签颜色" "hilited_label_color" Nothing model.hilited_label_color (\i -> UpdateColorScheme { model | hilited_label_color = i })
        , colorInput "背景颜色" "hilited_candidate_back_color" Nothing model.hilited_candidate_back_color (\i -> UpdateColorScheme { model | hilited_candidate_back_color = i })
        , colorInput "阴影颜色" "hilited_candidate_shadow_color" Nothing model.hilited_candidate_shadow_color (\i -> UpdateColorScheme { model | hilited_candidate_shadow_color = i })
        , divider "其他候选项"
        , colorInput "文字颜色" "candidate_text_color" Nothing model.candidate_text_color (\i -> UpdateColorScheme { model | candidate_text_color = i })
        , colorInput "阴影颜色" "candidate_shadow_color" Nothing model.candidate_shadow_color (\i -> UpdateColorScheme { model | candidate_shadow_color = i })
        ]


styleInputView : Style -> Html Msg
styleInputView style =
    div []
        [ h4 [ class "title is-4" ] [ text "风格" ]
        , div [ class "notification is-info is-light" ] [ text "本节只展示了部分选项" ]
        , textInputDisabled "当前配色方案" "color_scheme" style.color_scheme
        , textInput "字体" "font_face" Nothing style.font_face (\i -> UpdateStyle { style | font_face = i })
        , numberInput "字号" "font_point" (Just "大小完全不一样，预览仅供参考。") style.font_point (\i -> UpdateStyle { style | font_point = i })
        , boolInput "横排候选" "horizontal" Nothing style.horizontal (\i -> UpdateStyle { style | horizontal = i })
        , boolInput "嵌入式" "inline_preedit" Nothing style.inline_preedit (\i -> UpdateStyle { style | inline_preedit = i })
        , div [ class "field", class "is-horizontal" ]
            [ label [ class "label", class "field-label", title "preedit_type" ] [ text "预输入类型" ]
            , div [ class "field-body" ]
                [ div [ class "control" ]
                    [ div [ class "select" ]
                        [ select [ value (preEditTypeToString style.preedit_type) ]
                            [ option [ value "composition", onClick (UpdateStyle { style | preedit_type = Composition }) ] [ text "composition" ]
                            , option [ value "preview", onClick (UpdateStyle { style | preedit_type = Preview }) ] [ text "preview" ]
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
        , numberInput "高亮横向内边距" "hilite_padding_x" Nothing layout.hilite_padding_x (\i -> UpdateLayout { layout | hilite_padding_x = i })
        , numberInput "高亮纵向内边距" "hilite_padding_y" Nothing layout.hilite_padding_y (\i -> UpdateLayout { layout | hilite_padding_y = i })
        , numberInput "高亮背景圆角" "round_corner" Nothing layout.round_corner (\i -> UpdateLayout { layout | round_corner = i })
        , numberInput "横向阴影" "shadow_offset_x" Nothing layout.shadow_offset_x (\i -> UpdateLayout { layout | shadow_offset_x = i })
        , numberInput "纵向阴影" "shadow_offset_y" Nothing layout.shadow_offset_y (\i -> UpdateLayout { layout | shadow_offset_y = i })
        , numberInput "阴影半径" "shadow_radius" Nothing layout.shadow_radius (\i -> UpdateLayout { layout | shadow_radius = i })
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


hero : Html msg
hero =
    section [ class "hero is-info" ]
        [ div [ class "hero-body" ]
            [ p [ class "title" ] [ text "小狼毫皮肤编辑器" ]
            , p [ class "subtitle" ] [ text "预览仅供参考 (～￣▽￣)～  v0.16.3 beta" ]
            ]
        ]


docView : Html msg
docView =
    div [ class "content" ]
        [ h4 [ class "title is-4" ] [ text "使用说明" ]
        , ul []
            [ li [] [ text "设定好主题后复制“输出”栏的 yaml 输出。" ]
            , li [] [ text "点击托盘上，输入法图标旁的", code [] [ text "中" ], text "图标，选择", code [] [ text "用户文件夹" ] ]
            , li [] [ text "找到", code [] [ text "weasel.custom.yaml" ], text "，用文本编辑器打开并将复制的内容整合进 yaml 内的", code [] [ text "patch" ], text "选项下（注意缩进，patch 出现一次即可）。" ]
            , li [] [ text "重新部署。" ]
            , li [] [ text "设定细节详阅官方的 ", a [ href "https://github.com/rime/home/wiki/CustomizationGuide#%E5%AE%9A%E8%A3%BD%E6%8C%87%E5%8D%97", target "_blank" ] [ text "定制指南" ], text " 。" ]
            ]
        ]


footerView : Html msg
footerView =
    footer [ class "footer" ]
        [ div [ class "content has-text-centered" ]
            [ p [] [ text "2022-2025 ✨ OwlZou | ", a [ href "https://github.com/owlzou/weasel-theme-editor", target "_blank" ] [ text "Github" ] ]
            ]
        ]


view : Model -> Document Msg
view model =
    { title = "小狼毫皮肤编辑器"
    , body =
        [ hero
        , section
            [ class "section" ]
            [ div [ class "tile is-ancestor is-vertical" ]
                [ div [ class "tile", class "rime_options" ]
                    [ div [ class "tile is-parent is-vertical" ]
                        [ div [ class "preview-placeholder" ] []
                        , div [ class "tile is-child box preview", style "flex-grow" "0" ] [ preview model ]
                        , div [ class "tile is-child box yaml" ] [ yamlOutput model ]
                        ]
                    , div [ class "tile is-parent" ] [ div [ class "tile is-child box" ] [ Html.Lazy.lazy colorSchemeInputView model.color_scheme ] ]
                    , div [ class "tile is-parent" ] [ div [ class "tile is-child box" ] [ Html.Lazy.lazy styleInputView model.style, Html.Lazy.lazy layoutInputView model.layout ] ]
                    ]
                , div [ class "tile is-parent" ] [ div [ class "tile is-child box" ] [ docView ] ]
                ]
            ]
        , footerView
        ]
    }



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.batch [ yamlReceiver YAMLRecv, textSizeReceiver SizeRecv ]



-- PORTS


port setYAMLSource : Encode.Value -> Cmd msg


port yamlReceiver : (String -> msg) -> Sub msg


port textSizeReceiver : (Encode.Value -> msg) -> Sub msg



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


colorInput : String -> String -> Maybe String -> String -> (String -> Msg) -> Html Msg
colorInput name tip help val updateFunc =
    div [ class "field", class "is-horizontal" ]
        [ label [ class "label", class "field-label", title tip ] [ text name ]
        , div [ class "field-body" ]
            [ div [ class "field", class "is-grouped" ]
                [ div [ class "control" ] [ input [ class "input", value val, placeholder tip, onInput updateFunc ] [], helpUI help ]
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
