port module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, a, code, div, footer, h4, input, label, li, nav, option, p, section, select, span, text, textarea, ul)
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
        [ h4 [ class "title is-4" ] [ text "??????" ]
        , -- ????????????
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
                        text "?????????pei se"

                    Preview ->
                        text "???????????????"
                ]
            ]
        , canvasView model
        ]


colorSchemeInputView : ColorScheme -> Html Msg
colorSchemeInputView model =
    div []
        [ h4 [ class "title is-4" ] [ text "????????????" ]
        , textInput "??????" "ID" (Just "?????????????????????????????????????????????") model.id (\i -> UpdateID i)
        , textInput "??????" "name" Nothing model.name (\i -> UpdateColorScheme { model | name = i })
        , textInput "??????" "author" Nothing model.author (\i -> UpdateColorScheme { model | author = i })
        , divider "????????????"
        , colorInput "????????????" "back_color" Nothing model.back_color (\i -> UpdateColorScheme { model | back_color = i })
        , colorInput "????????????" "border_color" (Just "???????????????????????????????????????????????????") model.border_color (\i -> UpdateColorScheme { model | border_color = i })
        , divider "????????????"
        , colorInput "????????????" "text_color" Nothing model.text_color (\i -> UpdateColorScheme { model | text_color = i })
        , colorInput "????????????" "hilited_text_color" Nothing model.hilited_text_color (\i -> UpdateColorScheme { model | hilited_text_color = i })
        , colorInput "????????????" "hilited_back_color" Nothing model.hilited_back_color (\i -> UpdateColorScheme { model | hilited_back_color = i })
        , divider "???????????????"
        , colorInput "????????????" "hilited_candidate_text_color" Nothing model.hilited_candidate_text_color (\i -> UpdateColorScheme { model | hilited_candidate_text_color = i })
        , colorInput "????????????" "hilited_comment_text_color" Nothing model.hilited_comment_text_color (\i -> UpdateColorScheme { model | hilited_comment_text_color = i })
        , colorInput "????????????" "hilited_label_color" Nothing model.hilited_label_color (\i -> UpdateColorScheme { model | hilited_label_color = i })
        , colorInput "????????????" "hilited_candidate_back_color" Nothing model.hilited_candidate_back_color (\i -> UpdateColorScheme { model | hilited_candidate_back_color = i })
        , divider "???????????????"
        , colorInput "????????????" "candidate_text_color" Nothing model.candidate_text_color (\i -> UpdateColorScheme { model | candidate_text_color = i })
        , colorInput "????????????" "comment_text_color" Nothing model.comment_text_color (\i -> UpdateColorScheme { model | comment_text_color = i })
        , colorInput "????????????" "label_color" Nothing model.label_color (\i -> UpdateColorScheme { model | label_color = i })
        ]


styleInputView : Style -> Html Msg
styleInputView style =
    div []
        [ h4 [ class "title is-4" ] [ text "??????" ]
        , textInputDisabled "??????????????????" "color_scheme" style.color_scheme
        , textInput "??????" "font_face" Nothing style.font_face (\i -> UpdateStyle { style | font_face = i })
        , numberInput "??????" "font_point" (Just "?????????????????????????????????????????????") style.font_point (\i -> UpdateStyle { style | font_point = i })
        , boolInput "????????????" "horizontal" Nothing style.horizontal (\i -> UpdateStyle { style | horizontal = i })
        , boolInput "??????" "fullscreen" (Just "??????????????????????????????????????????") style.fullscreen (\i -> UpdateStyle { style | fullscreen = i })
        , boolInput "?????????" "inline_preedit" Nothing style.inline_preedit (\i -> UpdateStyle { style | inline_preedit = i })
        , div [ class "field", class "is-horizontal" ]
            [ label [ class "label", class "field-label", title "preedit_type" ] [ text "???????????????" ]
            , div [ class "field-body" ]
                [ div [ class "control" ]
                    [ div [ class "select" ]
                        [ select [ value (preEditTypeToString style.preedit_type) ]
                            [ option [ value "composition", onClick (UpdateStyle { style | preedit_type = Composition }) ] [ text "composition / ??????" ]
                            , option [ value "preview", onClick (UpdateStyle { style | preedit_type = Preview }) ] [ text "preview / ??????" ]
                            ]
                        ]
                    ]
                ]
            ]
        , boolInput "??????????????????" "display_tray_icon" Nothing style.display_tray_icon (\i -> UpdateStyle { style | display_tray_icon = i })
        , textInput "????????????" "label_format" (Just "%s ????????????") style.label_format (\i -> UpdateStyle { style | label_format = i })
        ]


layoutInputView : Layout -> Html Msg
layoutInputView layout =
    div []
        [ divider "??????"
        , numberInput "????????????" "min_width" Nothing layout.min_width (\i -> UpdateLayout { layout | min_width = i })
        , numberInput "????????????" "min_height" Nothing layout.min_height (\i -> UpdateLayout { layout | min_height = i })
        , numberInput "????????????" "border_width" Nothing layout.border_width (\i -> UpdateLayout { layout | border_width = i })
        , numberInput "???????????????" "margin_x" Nothing layout.margin_x (\i -> UpdateLayout { layout | margin_x = i })
        , numberInput "???????????????" "margin_y" Nothing layout.margin_y (\i -> UpdateLayout { layout | margin_y = i })
        , numberInput "?????????????????????" "spacing" Nothing layout.spacing (\i -> UpdateLayout { layout | spacing = i })
        , numberInput "???????????????" "candidate_spacing" Nothing layout.candidate_spacing (\i -> UpdateLayout { layout | candidate_spacing = i })
        , numberInput "???????????????" "hilite_spacing" Nothing layout.hilite_spacing (\i -> UpdateLayout { layout | hilite_spacing = i })
        , numberInput "?????????????????????" "hilite_padding" Nothing layout.hilite_padding (\i -> UpdateLayout { layout | hilite_padding = i })
        , numberInput "??????????????????" "round_corner" Nothing layout.round_corner (\i -> UpdateLayout { layout | round_corner = i })
        ]


yamlOutput : Model -> Html Msg
yamlOutput model =
    div [ class "field" ]
        [ h4 [ class "title is-4" ] [ text "??????" ]
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
            [ p [ class "title" ] [ text "????????????????????????" ]
            , p [ class "subtitle" ] [ text "?????????????????? (????????????)???" ]
            ]
        ]


docView : Html msg
docView =
    div [ class "content" ]
        [ h4 [ class "title is-4" ] [ text "????????????" ]
        , ul []
            [ li [] [ text "?????????????????????????????????????????? yaml ?????????" ]
            , li [] [ text "???????????????????????????????????????", code [] [ text "???" ], text "???????????????", code [] [ text "???????????????" ] ]
            , li [] [ text "??????", code [] [ text "weasel.custom.yaml" ], text "????????????????????????????????????????????????????????? yaml ??????", code [] [ text "patch" ], text "???????????????????????????patch ????????????????????????" ]
            , li [] [ text "???????????????" ]
            , li [] [ text "??????????????????????????? ", a [ href "https://github.com/rime/home/wiki/CustomizationGuide#%E5%AE%9A%E8%A3%BD%E6%8C%87%E5%8D%97", target "_blank" ] [ text "????????????" ], text " ???" ]
            ]
        ]


footerView : Html msg
footerView =
    footer [ class "footer" ]
        [ div [ class "content has-text-centered" ]
            [ p [] [ text "2022 ??? OwlZou | ", a [ href "https://github.com/owlzou/weasel-theme-editor", target "_blank" ] [ text "Github" ] ]
            ]
        ]


view : Model -> Document Msg
view model =
    { title = "????????????????????????"
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
