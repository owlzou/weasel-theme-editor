module Render exposing (canvasView)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced
import Canvas.Settings.Text exposing (TextBaseLine(..), baseLine, font)
import Color
import ColorHelper exposing (blendColor, hex2ColorFallback, hex2ColorWithDefault, transparentColor)
import Html exposing (Html)
import Model exposing (Model, PreEditType(..), getLabel)


str2Float : Float -> String -> Float
str2Float default str =
    Maybe.withDefault default (String.toFloat str)


roundRect : Point -> Float -> Float -> Float -> Shape
roundRect ( x, y ) w h r =
    Canvas.path ( x + r, y )
        [ arcTo ( x + w, y ) ( x + w, y + h ) r
        , arcTo ( x + w, y + h ) ( x, y + h ) r
        , arcTo ( x, y + h ) ( x, y ) r
        , arcTo ( x, y ) ( x + w, y ) r
        ]


canvasView : Model -> Html msg
canvasView model =
    let
        textFullWidth =
            model.textSize.width

        textFullHeight =
            model.textSize.height

        borderWidth =
            str2Float 0 model.layout.border_width

        candidateSpacing =
            if model.style.horizontal then
                max (str2Float 0 model.layout.candidate_spacing) hilitePaddingX * 2

            else
                max (str2Float 0 model.layout.candidate_spacing) hilitePaddingY * 2

        hiliteSpacing =
            str2Float 0 model.layout.hilite_spacing

        hilitePaddingX =
            str2Float 0 model.layout.hilite_padding_x

        hilitePaddingY =
            str2Float 0 model.layout.hilite_padding_y

        marginX =
            max (str2Float 0 model.layout.margin_x) hilitePaddingX + borderWidth / 2

        marginY =
            max (str2Float 0 model.layout.margin_y) hilitePaddingY + borderWidth / 2

        spacing =
            str2Float 0 model.layout.spacing

        shadow_offset_x =
            str2Float 0 model.layout.shadow_offset_x

        shadow_offset_y =
            str2Float 0 model.layout.shadow_offset_y

        shadow_radius =
            str2Float 0 model.layout.shadow_radius

        roundCorner =
            min (str2Float 0 model.layout.round_corner) (model.textSize.height / 2)

        -- 计算长度
        candidateWidth =
            model.textSize.labelWidth + textFullWidth + hiliteSpacing * 2 + model.textSize.commentWidth
        
        candidateHeight = textFullHeight + marginY * 2

        candidateFirstWidth =
            model.textSize.labelWidth + textFullWidth * 2 + model.textSize.fullCommentWidth + hiliteSpacing * 2

        width =
            let
                calcW =
                    if model.style.horizontal then
                        marginX * 2 + candidateFirstWidth + candidateWidth * 4 + candidateSpacing * 4

                    else
                        marginX * 2 + candidateFirstWidth
            in
            max calcW (str2Float 0 model.layout.min_width)

        height =
            let
                calcH =
                    if model.style.inline_preedit then
                        if model.style.horizontal then
                            textFullHeight + marginY * 2

                        else
                            textFullHeight * 5 + marginY * 2 + candidateSpacing * 4

                    else if model.style.horizontal then
                        textFullHeight * 2 + marginY * 2 + spacing

                    else
                        textFullHeight * 6 + marginY * 2 + spacing + candidateSpacing * 4
            in
            max calcH (str2Float 0 model.layout.min_height)

        fullWidth =
            width + shadow_radius + shadow_offset_x

        fullHeight =
            height + shadow_radius + shadow_offset_y

        yStart =
            if model.style.inline_preedit then
                marginY

            else
                marginY + textFullHeight + spacing

        -- 颜色Fallback
        textColor =
            hex2ColorWithDefault Color.black model.color_scheme.text_color

        backColor =
            hex2ColorWithDefault Color.white model.color_scheme.back_color

        candidateTextColor =
            hex2ColorWithDefault textColor model.color_scheme.candidate_text_color

        borderColor =
            hex2ColorWithDefault textColor model.color_scheme.border_color

        hilitedTextColor =
            hex2ColorWithDefault textColor model.color_scheme.hilited_text_color

        hilitedCandidateTextColor =
            hex2ColorWithDefault hilitedTextColor model.color_scheme.hilited_candidate_text_color

        hilitedBackColor =
            hex2ColorWithDefault backColor model.color_scheme.hilited_back_color

        hilitedCandidateBackColor =
            hex2ColorWithDefault hilitedBackColor model.color_scheme.hilited_candidate_back_color

        labelColor =
            hex2ColorWithDefault (blendColor candidateTextColor backColor) model.color_scheme.label_color

        commentTextColor =
            hex2ColorWithDefault labelColor model.color_scheme.comment_text_color

        hilitedLabelColor =
            hex2ColorWithDefault (blendColor hilitedCandidateTextColor hilitedCandidateBackColor) model.color_scheme.hilited_label_color

        hilitedCommentTextColor =
            hex2ColorFallback hilitedLabelColor model.color_scheme.comment_text_color model.color_scheme.hilited_comment_text_color

        shadowColor =
            hex2ColorWithDefault transparentColor model.color_scheme.shadow_color

        candidateShadowColor =
            hex2ColorWithDefault transparentColor model.color_scheme.candidate_shadow_color

        hilitedShadowColor =
            hex2ColorWithDefault transparentColor model.color_scheme.hilited_shadow_color

        hilitedCandidateShadowColor =
            hex2ColorWithDefault transparentColor model.color_scheme.hilited_candidate_shadow_color

        defaultShadow =
            { blur = shadow_radius, color = shadowColor, offset = ( shadow_offset_x, shadow_offset_y ) }

        -- 绘制输入框
        drawInput =
            if model.style.inline_preedit then
                []

            else
                case model.style.preedit_type of
                    Composition ->
                        [ -- 文字
                          drawText textColor ( marginX, marginY ) "小狼毫"

                        -- 编码背景
                        , shapes [ fill hilitedBackColor, Canvas.Settings.Advanced.shadow { defaultShadow | color = hilitedShadowColor } ]
                            [ roundRect
                                ( marginX + textFullWidth * 3 + hiliteSpacing - hilitePaddingX, marginY - hilitePaddingY )
                                (model.textSize.fullCommentWidth + hilitePaddingX * 2)
                                (textFullHeight + hilitePaddingY * 2)
                                roundCorner
                            ]
                        , drawText hilitedTextColor ( marginX + textFullWidth * 3 + hiliteSpacing, marginY ) "pei se"
                        ]

                    Preview ->
                        [ -- 编码背景
                          shapes [ fill hilitedBackColor, Canvas.Settings.Advanced.shadow { defaultShadow | color = hilitedShadowColor } ]
                            [ roundRect
                                ( marginX - hilitePaddingX, marginY - hilitePaddingY )
                                (textFullWidth * 5 + hilitePaddingX * 2)
                                (textFullHeight + hilitePaddingY * 2)
                                roundCorner
                            ]
                        , drawText hilitedTextColor ( marginX, marginY ) "小狼毫配色"
                        ]

        -- 绘制候选项（非第一个）
        drawCandidate : Int -> String -> String -> List Renderable
        drawCandidate idx text comment =
            let
                h =
                    if model.style.inline_preedit then
                        marginY + textFullHeight * (toFloat idx - 1) + candidateSpacing * (toFloat idx - 1)

                    else
                        marginY + textFullHeight * toFloat idx + spacing + candidateSpacing * (toFloat idx - 1)

                horizontalH =
                    if model.style.inline_preedit then
                        marginY

                    else
                        marginY + textFullHeight + spacing

                horizontalW =
                    marginX + candidateFirstWidth + candidateWidth * (toFloat idx - 2) + candidateSpacing * (toFloat idx - 1)


                drawCandidateShadow x y =
                    shapes
                        [ fill candidateShadowColor
                        , Canvas.Settings.Advanced.shadow
                            { blur = shadow_radius
                            , color = candidateShadowColor
                            , offset = ( x + candidateWidth + shadow_offset_x, y + textFullHeight + marginY + shadow_offset_y )
                            }
                        ]
                        [ rect ( -candidateWidth, -candidateHeight ) candidateWidth candidateHeight ]
            in
            if model.style.horizontal then
                [ drawCandidateShadow horizontalW horizontalH
                , drawText labelColor ( horizontalW, horizontalH ) (getLabel idx model.style.label_format)
                , drawText candidateTextColor ( horizontalW + model.textSize.labelWidth + hiliteSpacing, horizontalH ) text
                , drawText commentTextColor ( horizontalW + model.textSize.labelWidth + textFullWidth + hiliteSpacing * 2, horizontalH ) comment
                ]

            else
                [ drawCandidateShadow marginX h
                , drawText labelColor ( marginX, h ) (getLabel idx model.style.label_format)
                , drawText candidateTextColor ( marginX + model.textSize.labelWidth + hiliteSpacing, h ) text
                , drawText commentTextColor ( marginX + model.textSize.labelWidth + textFullWidth + hiliteSpacing * 2, h ) comment
                ]

        -- 绘制文字
        drawText color point text =
            Canvas.text
                [ font
                    { size = round (Maybe.withDefault 16 (String.toFloat model.style.font_point) * 1.5)
                    , family = "\"" ++ model.style.font_face ++ "\""
                    }
                , fill color
                , baseLine Top
                ]
                point
                text
    in
    Canvas.toHtml ( round fullWidth, round fullHeight )
        []
        (List.concat
            [ [ --清理
                clear ( 0, 0 ) fullWidth fullHeight

              -- 边框
              , shapes [ fill borderColor, Canvas.Settings.Advanced.shadow defaultShadow ] [ rect ( 0, 0 ) width height ]

              -- 背景
              , shapes [ fill backColor ] [ rect ( min borderWidth (width / 2), min borderWidth (height / 2) ) (width - borderWidth * 2) (height - borderWidth * 2) ]
              ]
            , drawInput
            , [ -- 第一个候选高亮背景
                shapes [ fill hilitedCandidateBackColor, Canvas.Settings.Advanced.shadow { defaultShadow | color = hilitedCandidateShadowColor } ]
                    [ roundRect
                        ( marginX - hilitePaddingX, yStart - hilitePaddingY )
                        (candidateFirstWidth + hilitePaddingX * 2)
                        (textFullHeight + hilitePaddingY * 2)
                        roundCorner
                    ]

              -- 第一个候选词
              , drawText hilitedLabelColor ( marginX, yStart ) (getLabel 1 model.style.label_format)
              , drawText hilitedCandidateTextColor ( marginX + model.textSize.labelWidth + hiliteSpacing, yStart ) "配色"
              , drawText hilitedCommentTextColor ( marginX + model.textSize.labelWidth + textFullWidth * 2 + hiliteSpacing * 2, yStart ) "pei se"
              ]
            , drawCandidate 2 "陪" "pei"
            , drawCandidate 3 "配" "pei"
            , drawCandidate 4 "赔" "pei"
            , drawCandidate 5 "培" "pei"
            ]
        )
