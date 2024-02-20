local wezterm = require 'wezterm'

return {
    window_decorations = "RESIZE",
    font = wezterm.font 'JetBrains Mono',
    font_size = 15,
    line_height = 1.1,
    hide_tab_bar_if_only_one_tab = true,
    bypass_mouse_reporting_modifiers = "SHIFT",
    send_composed_key_when_left_alt_is_pressed = true,
    send_composed_key_when_right_alt_is_pressed = true,
}
