# 参考（更新至`0.16.3`版本）

- [小狼毫默认皮肤](https://github.com/rime/weasel/blob/0.16.3/output/data/weasel.yaml)
- [使用的 UI 选项](https://github.com/rime/weasel/blob/0.16.3/include/WeaselIPCData.h)
- [横向 UI 绘制](https://github.com/rime/weasel/blob/0.16.3/WeaselUI/HorizontalLayout.cpp)
- [更新日志](https://github.com/rime/weasel/blob/0.16.3/CHANGELOG.md)
- 边框长度在总长度之内，即使设为 0 也会有 1 像素，无边框效果需要将边框设成和背景一个颜色

## 颜色的 Fallback

参考 [这里](https://github.com/rime/weasel/blob/0.16.3/RimeWithWeasel/RimeWithWeasel.cpp):

- `candidate_text_color` → `text_color`
- `border_color` → `text_color`
- `hilited_candidate_text_color` → `hilited_text_color` → `text_color`
- `hilited_candidate_back_color` → `hilited_back_color` → `back_color`
- `comment_text_color` → `label_color` → 混合 `candidate_text_color` 和 `back_color`
- `hilited_comment_text_color` → `comment_text_color` → `hilited_label_color` → 混合 `hilited_candidate_text_color` 和 `hilited_candidate_back_color`


## 预定增加参数

- [X] `preset_color_schemes/color_scheme/shadow_color: color`
- [] `preset_color_schemes/color_scheme/nextpage_color: color`
- [] `preset_color_schemes/color_scheme/prevpage_color: color`
- [] `preset_color_schemes/color_scheme/candidate_back_color: color`
- [X] `preset_color_schemes/color_scheme/candidate_shadow_color: color`
- [] `preset_color_schemes/color_scheme/candidate_border_color: color`
- [X] `preset_color_schemes/color_scheme/hilited_shadow_color: color`
- [X] `preset_color_schemes/color_scheme/hilited_candidate_shadow_color: color`
- [] `preset_color_schemes/color_scheme/hilited_candidate_border_color: color`
- [] `preset_color_schemes/color_scheme/hilited_mark_color: color`

（下面的可能不会添加了）

- [X] `style/layout/hilite_padding_x`, `style/layout/hilite_padding_y`
- [] `style/text_orientation: "horizontal" | "vertical"`
- [] `style/layout/baseline: int`, `style/layout/linespacing: int`
- [X] `style/layout/shadow_radius: int`
- [X] `style/layout/shadow_offset_x: int`,`style/layout/shadow_offset_y: int`
- [] `style/label_font_face: string`
- [] `style/comment_font_face: string`
- [] `style/label_font_point: int`
- [] `style/comment_font_point: int`
- [] `style/layout/align_type: "top" | "center" | "bottom"`
- [] `style/vertical_text: bool`
- [] `style/vertical_text_left_to_right: bool`
- [] `style/vertical_text_with_wrap: bool`
- [] `style/preedit_type: "composition" | "preview" | "preview_all"`
- [] `style/mark_text: string`