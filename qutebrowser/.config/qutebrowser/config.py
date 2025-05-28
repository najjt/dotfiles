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
c.hints.selectors = {'all': ['a', 'area', 'textarea', 'select', 'input:not([type="hidden"])', 'button', 'frame', 'iframe', 'img', 'link', 'summary', '[contenteditable]:not([contenteditable="false"])', '[onclick]', '[onmousedown]', '[role="link"]', '[role="option"]', '[role="button"]', '[role="tab"]', '[role="checkbox"]', '[role="menuitem"]', '[role="menuitemcheckbox"]', '[role="menuitemradio"]', '[role="treeitem"]', '[aria-haspopup]', '[ng-click]', '[ngClick]', '[data-ng-click]', '[x-ng-click]', '[tabindex]:not([tabindex="-1"])'], 'links': ['a[href]', 'area[href]', 'link[href]', '[role="link"][href]'], 'images': ['img'], 'media': ['audio', 'img', 'video'], 'url': ['[src]', '[href]'], 'inputs': ['input[type="text"]', 'input[type="date"]', 'input[type="datetime-local"]', 'input[type="email"]', 'input[type="month"]', 'input[type="number"]', 'input[type="password"]', 'input[type="search"]', 'input[type="tel"]', 'input[type="time"]', 'input[type="url"]', 'input[type="week"]', 'input:not([type])', '[contenteditable]:not([contenteditable="false"])', 'textarea']}

# Languages to use for spell checking. You can check for available
# languages and install dictionaries using scripts/dictcli.py. Run the
# script with -h/--help for instructions.
c.spellcheck.languages = ['sv-SE', 'en-US']

# How to behave when the last tab is closed.
c.tabs.last_close = 'ignore'

# Save tabs on quit/restart
c.auto_save.session = True

# Search engines which can be used via the address bar.
c.url.searchengines = {'DEFAULT': 'https://www.google.com/search?q={}', 'aw': 'https://wiki.archlinux.org/?search={}', 'w': 'https://en.wikipedia.org/?search={}', 'wt': 'https://en.wiktionary.org/?search={}', 'l': 'https://letterboxd.com/search/{}', 'yt': 'https://www.youtube.com/results?search_query={}'}

# Page(s) to open at the start.
c.url.start_pages = 'https://qutebrowser.com/'

# Page to open if :open -t/-b/-w is used without URL.
c.url.default_page = 'about:blank'

# Privacy
config.set("content.webgl", False, "*")
config.set("content.canvas_reading", False)
config.set("content.geolocation", False)
config.set("content.webrtc_ip_handling_policy", "default-public-interface-only")
config.set("content.cookies.accept", "all")
config.set("content.cookies.store", True)

#
# Appearance
#

# Position of the status bar.
c.statusbar.position = 'bottom'

# Position of the tab bar.
c.tabs.position = 'top'

# When to show the tab bar.
c.tabs.show = 'multiple'

# Format to use for the tab title.
c.tabs.title.format = '{audio}{current_title}'

# When/how to show the scrollbar.
c.scrolling.bar = 'never'

# Hide the window decoration.
c.window.hide_decoration = False

# Format to use for the window title.
c.window.title_format = '{current_title} - qutebrowser'


# Colors

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
c.colors.tabs.odd.bg = '#1e1e1e'

# Foreground color of unselected even tabs.
c.colors.tabs.even.fg = '#b7bec9'

# Background color of unselected even tabs.
c.colors.tabs.even.bg = '#343434'

# Foreground color of selected odd tabs.
c.colors.tabs.selected.odd.fg = '#000000'

# Background color of selected odd tabs.
c.colors.tabs.selected.odd.bg = '#cecece'

# Foreground color of selected even tabs.
c.colors.tabs.selected.even.fg = '#000000'

# Background color of selected even tabs.
c.colors.tabs.selected.even.bg = '#cecece'

# Background color of the statusbar in private browsing mode.
c.colors.statusbar.private.bg = '#6232a8'

# Prefer dark mode
c.colors.webpage.preferred_color_scheme = 'dark'

# Font
c.fonts.default_family = []
c.fonts.default_size = '12pt'
c.fonts.web.size.default = 18
c.fonts.web.family.fixed = 'monospace'
c.fonts.web.family.sans_serif = 'sans'
c.fonts.web.family.serif = 'serif'
c.fonts.web.family.standard = 'sans'

# Tabs
c.tabs.padding = {'top': 2, 'bottom': 2, 'left': 9, 'right': 9}
c.tabs.indicator.width = 0 # no tab indicators
c.tabs.width = '7%'

# Statusbar
c.statusbar.padding = {'top': 2, 'bottom': 2, 'left': 9, 'right': 9}

c.editor.command = ['emacsclient', '-c', '+{line}:{column}', '{file}']

#
# Keybindings
#

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
