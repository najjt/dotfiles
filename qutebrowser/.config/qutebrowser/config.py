# Change the argument to True to still load settings configured via autoconfig.yml
config.load_autoconfig(False)

# Set PATH
import os
os.environ['PATH'] = '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/najjt/.local/bin'

# Automatically start playing `<video>` elements.
c.content.autoplay = False

# Allow websites to show notifications.
c.content.notifications.enabled = False

# CSS selectors used to determine which elements on a page should have
# hints.
c.hints.selectors = {'all': ['a', 'area', 'textarea', 'select', 'input:not([type="hidden"])', 'button', 'frame', 'iframe', 'img', 'link', 'summary', '[contenteditable]:not([contenteditable="false"])', '[onclick]', '[onmousedown]', '[role="link"]', '[role="option"]', '[role="button"]', '[role="tab"]', '[role="checkbox"]', '[role="menuitem"]', '[role="menuitemcheckbox"]', '[role="menuitemradio"]', '[role="treeitem"]', '[aria-haspopup]', '[ng-click]', '[ngClick]', '[data-ng-click]', '[x-ng-click]', '[tabindex]:not([tabindex="-1"])'], 'links': ['a[href]', 'area[href]', 'link[href]', '[role="link"][href]'], 'images': ['img'], 'media': ['audio', 'img', 'video'], 'url': ['[src]', '[href]'], 'inputs': ['input[type="text"]', 'input[type="date"]', 'input[type="datetime-local"]', 'input[type="email"]', 'input[type="month"]', 'input[type="number"]', 'input[type="password"]', 'input[type="search"]', 'input[type="tel"]', 'input[type="time"]', 'input[type="url"]', 'input[type="week"]', 'input:not([type])', '[contenteditable]:not([contenteditable="false"])', 'textarea'], 'code': [':not(pre) > code', 'pre']}

# When/how to show the scrollbar.
c.scrolling.bar = 'never'

# Languages to use for spell checking. You can check for available
# languages and install dictionaries using scripts/dictcli.py. Run the
# script with -h/--help for instructions.
c.spellcheck.languages = ['sv-SE', 'en-US']

# Position of the status bar.
c.statusbar.position = 'bottom'

# How to behave when the last tab is closed.
c.tabs.last_close = 'ignore'

# Position of the tab bar.
c.tabs.position = 'top'

# When to show the tab bar.
c.tabs.show = 'multiple'

# Format to use for the tab title.
c.tabs.title.format = '{audio}{current_title}'

# Search engines which can be used via the address bar.
c.url.searchengines = {'DEFAULT': 'https://www.google.com/search?q={}', 'aw': 'https://wiki.archlinux.org/?search={}', 'w': 'https://en.wikipedia.org/?search={}', 'wt': 'https://en.wiktionary.org/?search={}', 'l': 'https://letterboxd.com/search/{}', 'yt': 'https://www.youtube.com/results?search_query={}'}

# Page(s) to open at the start.
c.url.start_pages = 'about:blank'

# Page to open if :open -t/-b/-w is used without URL.
c.url.default_page = 'about:blank'

# Hide the window decoration.
c.window.hide_decoration = False

# Format to use for the window title.
c.window.title_format = '{current_title} - qutebrowser'

# Default zoom level.
c.zoom.default = '110%'


#
# Colors
#

# Foreground color of the statusbar.
c.colors.statusbar.normal.fg = '#b7bec9'

# Background color of the statusbar.
c.colors.statusbar.normal.bg = '#000000'

# Foreground color of the statusbar in command mode.
c.colors.statusbar.command.fg = '#b7bec9'

# Background color of the statusbar in command mode.
c.colors.statusbar.command.bg = '#000000'

# Color gradient end for the tab indicator.
c.colors.tabs.indicator.stop = '#638e8a'

# Foreground color of unselected odd tabs.
c.colors.tabs.odd.fg = '#b7bec9'

# Background color of unselected odd tabs.
c.colors.tabs.odd.bg = '#000000'

# Foreground color of unselected even tabs.
c.colors.tabs.even.fg = '#b7bec9'

# Background color of unselected even tabs.
c.colors.tabs.even.bg = '#000000'

# Background color of selected odd tabs.
c.colors.tabs.selected.odd.bg = '#747474'

# Background color of selected even tabs.
c.colors.tabs.selected.even.bg = '#747474'

# Background color of the statusbar in private browsing mode.
c.colors.statusbar.private.bg = '#6232a8'

# Value to use for `prefers-color-scheme:` for websites.
c.colors.webpage.preferred_color_scheme = 'dark'

c.fonts.default_family = 'Liberation Mono'

c.fonts.default_size = '12pt'

c.fonts.web.size.default = 11

c.tabs.padding = {'top': 2, 'bottom': 2, 'left': 9, 'right': 9}
c.tabs.indicator.width = 0 # no tab indicators
c.tabs.width = '7%'

c.statusbar.padding = {'top': 2, 'bottom': 2, 'left': 9, 'right': 9}

c.editor.command = ['emacsclient', '+{line}:{column}', '{file}']

#
# Keybindings for normal mode
#

# Unbind keys used in window manager
config.unbind('<Alt+1>')
config.unbind('<Alt+2>')
config.unbind('<Alt+3>')
config.unbind('<Alt+4>')
config.unbind('<Alt+5>')
config.unbind('<Alt+6>')
config.unbind('<Alt+7>')
config.unbind('<Alt+8>')
config.unbind('<Alt+9>')

# Unbind <CTRL-w>
config.unbind('<Ctrl+w>')

# Help shortcut
config.bind('<Ctrl+h>', 'cmd-set-text -s :help')

# Open new window in private browsing mode
config.bind('<Ctrl+p>', 'open -p')

# Rebind default zoom in key
config.bind('=', 'zoom-in')

# Edit url
config.bind('eu', 'edit-url')

# Open download
config.bind('ed', 'download-open')

# Yank to Org Mode link format
config.bind('yo', 'yank inline [[{url}][{title}]]')

# Allow clipboard access for 10 s
config.bind('ca', 'set -t content.javascript.clipboard access ;; cmd-later 10s set -p content.javascript.clipboard none')

#
# Userscripts
#

# mpv
config.bind('zM', 'hint links spawn mpv {hint-url}')
config.bind('zm', 'spawn mpv {url}')

# Bitwarden
config.bind('zl', 'spawn --userscript qute-bitwarden.sh')

# Code select
config.bind('zc', 'hint code userscript code_select.py')
