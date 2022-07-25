## 参考

- [小狼毫默认皮肤](https://github.com/rime/weasel/blob/371e08e4c799beef15ec93b461b17125c77f166b/output/data/weasel.yaml)
- [使用的 UI 选项](https://github.com/rime/weasel/blob/371e08e4c799beef15ec93b461b17125c77f166b/include/WeaselCommon.h)
- [横向 UI 绘制](https://github.com/rime/weasel/blob/371e08e4c799beef15ec93b461b17125c77f166b/WeaselUI/HorizontalLayout.cpp)
- 边框长度在总长度之内，即使设为 0 也会有 1 像素，无边框效果需要将边框设成和背景一个颜色

## 颜色的 Fallback

参考 [这里](https://github.com/rime/weasel/blob/371e08e4c799beef15ec93b461b17125c77f166b/RimeWithWeasel/RimeWithWeasel.cpp):

- `candidate_text_color` → `text_color`
- `border_color` → `text_color`
- `hilited_candidate_text_color` → `hilited_text_color` → `text_color`
- `hilited_candidate_back_color` → `hilited_back_color` → `back_color`
- `comment_text_color` → `label_color` → 混合 `candidate_text_color` 和 `back_color`
- `hilited_comment_text_color` → `comment_text_color` → `hilited_label_color` → 混合 `hilited_candidate_text_color` 和 `hilited_candidate_back_color`

