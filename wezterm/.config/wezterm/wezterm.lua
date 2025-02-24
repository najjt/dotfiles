local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.font = wezterm.font('Noto Sans Mono')
config.font_size = 12.0
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.hide_tab_bar_if_only_one_tab = true

config.colors = {
   background = '#232627',
}

local wezterm = require 'wezterm'
local act = wezterm.action

config.keys = {
  -- activate pane selection mode with the default alphabet (labels are "a", "s", "d", "f" and so on)
  { key = '8', mods = 'CTRL', action = act.PaneSelect },
  -- activate pane selection mode with numeric labels
  {
    key = '9',
    mods = 'CTRL',
    action = act.PaneSelect {
      alphabet = '1234567890',
    },
  },
  -- show the pane selection mode, but have it swap the active and selected panes
  {
    key = '0',
    mods = 'CTRL',
    action = act.PaneSelect {
      mode = 'SwapWithActive',
    },
  },
}

return config
