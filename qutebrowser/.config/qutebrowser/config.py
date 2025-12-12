# Change the argument to True to still load settings configured via autoconfig.yml
config.load_autoconfig(False)

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

# Search engines which can be used via the address bar.
c.url.searchengines = {'DEFAULT': 'https://google.com/search?q={}', 'aw': 'https://wiki.archlinux.org/?search={}', 'w': 'https://en.wikipedia.org/?search={}', 'wt': 'https://en.wiktionary.org/?search={}', 'l': 'https://letterboxd.com/search/{}', 'yt': 'https://www.youtube.com/results?search_query={}'}

# Page(s) to open at the start.
c.url.start_pages = 'https://qutebrowser.com/'

# Page to open if :open -t/-b/-w is used without URL.
c.url.default_page = 'about:blank'

# Privacy
config.set('content.webgl', False, '*')
config.set('content.canvas_reading', False)
config.set('content.geolocation', False)
config.set('content.webrtc_ip_handling_policy', 'default-public-interface-only')
config.set('content.cookies.accept', 'all')
config.set('content.cookies.store', True)

# Set a popular user-agent
config.set('content.headers.user_agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.3')

# Set a common accept_language header
config.set('content.headers.accept_language', 'en-US,en;q=0.5')

# Set a common HTTP_ACCEPT header:
config.set('content.headers.custom', {'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'})

c.editor.command = ['emacsclient', '-c', '+{line}:{column}', '{file}']

c.downloads.open_dispatcher = 'mimeopen'

config.set('scrolling.smooth', True)

#
# Appearance
#

# Position of the status bar.
c.statusbar.position = 'bottom'

# Position of the tab bar.
c.tabs.position = 'bottom'

# When to show the tab bar.
c.tabs.show = 'multiple'

# When/how to show the scrollbar.
c.scrolling.bar = 'never'

# Fonts
c.fonts.default_family        = []
c.fonts.default_size          = '11.5pt'
c.fonts.web.size.default      = 16
c.fonts.web.family.fixed      = 'monospace'
c.fonts.web.family.sans_serif = 'sans'
c.fonts.web.family.serif      = 'serif'
c.fonts.web.family.standard   = 'sans'


#
# Keybindings
#

# Help shortcut
config.bind('<Ctrl+h>', 'cmd-set-text -s :help')

# Open new window in private browsing mode
config.bind('<Ctrl+p>', 'open -p')

# Rebind default zoom in key
config.bind('=', 'zoom-in')

# Edit url
config.bind('eu', 'edit-url')

# Open an external editor with the currently selected form field
config.bind('<Ctrl-o>', 'edit-text', 'insert')

# Open download
config.bind('ed', 'download-open')

# Yank to Org Mode link format
config.bind('yo', 'yank inline [[{url}][{title}]]')

# Allow clipboard access for 10 s
config.bind('ca', 'set -t content.javascript.clipboard access ;; cmd-later 10s set -p content.javascript.clipboard none')

config.bind('<Ctrl-a>', 'fake-key <Home>', 'insert')
config.bind('<Ctrl-e>', 'fake-key <End>', 'insert')
config.bind('<Ctrl-b>', 'fake-key <Left>', 'insert')
config.bind('<Mod1-b>', 'fake-key <Ctrl-Left>', 'insert')
config.bind('<Ctrl-f>', 'fake-key <Right>', 'insert')
config.bind('<Mod1-f>', 'fake-key <Ctrl-Right>', 'insert')
config.bind('<Ctrl-p>', 'fake-key <Up>', 'insert')
config.bind('<Ctrl-n>', 'fake-key <Down>', 'insert')
config.bind('<Ctrl-w>', 'fake-key <Ctrl-Backspace>', 'insert')
