import os
{{ if eq .chezmoi.os "darwin" }}
os.environ['PATH'] = '/opt/homebrew/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/TeX/texbin:/opt/homebrew/bin:/opt/homebrew/sbin:/opt/local/sbin:/opt/local/bin:/usr/local/sicstus4.8.0/bin:/Applications/WezTerm.app/Contents/MacOS:/Users/najjt/Library/Python/3.9/bin:/Users/najjt/Projects/kod/scripts'

config.set('content.javascript.clipboard', 'access', 'dsv.su.se')
config.set('content.javascript.clipboard', 'access', 'nextilearn.dsv.su.se')
config.set('content.javascript.clipboard', 'access', 'nextilearn.se')
config.set('content.javascript.clipboard', 'access', 'sonofatailor.com')
config.set('content.javascript.clipboard', 'access', 'chat.openai.com')
config.set('content.javascript.clipboard', 'access', 'github.com')
{{ end }}

#
# General
#
config.load_autoconfig(False)
c.changelog_after_upgrade = 'major'
c.new_instance_open_target = 'tab'
c.qt.environ = {}
c.auto_save.interval = 15000
c.content.autoplay = False
c.content.notifications.enabled = False
c.content.cookies.accept = 'no-unknown-3rdparty'
config.set('content.register_protocol_handler', True, 'https://mail.proton.me#mailto=%25s')
c.editor.command = ['gvim', '-g', '-f', '{file}', '-c', 'normal {line}G{column0}l']
c.hints.scatter = False
c.hints.selectors = {'all': ['a', 'area', 'textarea', 'select', 'input:not([type="hidden"])', 'button', 'frame', 'iframe', 'img', 'link', 'summary', '[contenteditable]:not([contenteditable="false"])', '[onclick]', '[onmousedown]', '[role="link"]', '[role="option"]', '[role="button"]', '[role="tab"]', '[role="checkbox"]', '[role="menuitem"]', '[role="menuitemcheckbox"]', '[role="menuitemradio"]', '[role="treeitem"]', '[aria-haspopup]', '[ng-click]', '[ngClick]', '[data-ng-click]', '[x-ng-click]', '[tabindex]:not([tabindex="-1"])'], 'links': ['a[href]', 'area[href]', 'link[href]', '[role="link"][href]'], 'images': ['img'], 'media': ['audio', 'img', 'video'], 'url': ['[src]', '[href]'], 'inputs': ['input[type="text"]', 'input[type="date"]', 'input[type="datetime-local"]', 'input[type="email"]', 'input[type="month"]', 'input[type="number"]', 'input[type="password"]', 'input[type="search"]', 'input[type="tel"]', 'input[type="time"]', 'input[type="url"]', 'input[type="week"]', 'input:not([type])', '[contenteditable]:not([contenteditable="false"])', 'textarea'], 'code': [':not(pre) > code', 'pre']}

c.scrolling.smooth = True
c.spellcheck.languages = ['sv-SE', 'en-US']
c.tabs.last_close = 'close'
c.tabs.position = 'bottom'
c.url.default_page = 'about:blank'
c.url.searchengines = {'DEFAULT': 'https://www.google.com/search?q={}'}
c.url.start_pages = 'about:blank'
c.zoom.default = '125%'

# Cookies
config.set('content.cookies.accept', 'all', 'chrome-devtools://*')
config.set('content.cookies.accept', 'all', 'devtools://*')
config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')

# History
c.completion.web_history.max_items=100

# Javascript
config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

# Allow locally loaded documents to access remote URLs.
config.set('content.local_content_can_access_remote_urls', True, 'file:///Users/najjt/Library/Application%20Support/qutebrowser/userscripts/*')
config.set('content.local_content_can_access_file_urls', False, 'file:///Users/najjt/Library/Application%20Support/qutebrowser/userscripts/*')

#
# Appearance
#
c.scrolling.bar = 'never'
c.window.transparent = False
c.statusbar.show = 'always'
c.statusbar.position = 'bottom'
c.tabs.show = 'multiple'
c.tabs.title.format = '{index}: {audio}{current_title}'
{{ if eq .chezmoi.os "darwin" }}
c.window.hide_decoration = True
{{ else if eq .chezmoi.os "linux" }}
c.window.hide_decoration = False
{{ end }}
c.window.title_format = ' '

# Colors
c.colors.completion.fg = '#b7bec9'
c.colors.completion.odd.bg = '#000000'
c.colors.completion.even.bg = '#000000'
c.colors.completion.category.fg = '#b7bec9'
c.colors.completion.category.bg = '#000000'
c.colors.completion.item.selected.fg = '#b7bec9'
c.colors.completion.item.selected.bg = '#000000'
c.colors.statusbar.normal.fg = '#b7bec9'
c.colors.statusbar.normal.bg = '#000000'
c.colors.statusbar.command.fg = '#b7bec9'
c.colors.statusbar.command.bg = '#000000'
c.colors.tabs.indicator.stop = '#638e8a'
c.colors.tabs.odd.fg = '#b7bec9'
c.colors.tabs.odd.bg = '#000000'
c.colors.tabs.even.fg = '#b7bec9'
c.colors.tabs.even.bg = '#000000'
c.colors.tabs.selected.odd.bg = '#747474'
c.colors.tabs.selected.even.bg = '#747474'

# Fonts
c.fonts.default_family = 'JetBrains Mono'
c.fonts.web.size.default = 20
c.fonts.default_size = '15pt'

#
# Keybindings for normal mode
#

# Custom
{{ if eq .chezmoi.os "darwin" }}
config.bind('<Meta+x>', 'cmd-set-text :')
config.bind('<Ctrl+s>', 'cmd-set-text /')
{{ else if eq .chezmoi.os "linux" }}
config.bind('<Alt+x>', 'set-cmd-text :')
config.bind('<Ctrl+s>', 'set-cmd-text /')
{{ end }}
config.bind('eu', 'edit-url')
config.bind('yo', 'yank inline [[{url}][{title}]]') # Yank with org mode link formatting
config.bind('öd', 'download-open')
config.bind(',m', 'spawn mpv {url}')
config.bind(',M', 'hint links spawn mpv {hint-url}')
config.bind(',b', 'spawn --userscript bitwarden')
config.bind(',c', 'hint code userscript code_select.py')
{{ if eq .chezmoi.os "darwin" }}
config.bind(',j', 'set -u {domain} content.javascript.clipboard access')
{{ else if eq .chezmoi.os "linux" }}
config.bind(',j', 'set -u {domain} content.javascript.can_access_clipboard true')
{{ end }}

# custom group for code snippets
c.hints.selectors["code"] = [
    # Selects all code tags whose direct parent is not a pre tag
    ":not(pre) > code",
    "pre"
]

# Unbind conflicting and uwanted keys
config.unbind('<Alt+1>')
config.unbind('<Alt+2>')
config.unbind('<Alt+3>')
config.unbind('<Alt+4>')
config.unbind('<Alt+5>')
config.unbind('<Alt+6>')
config.unbind('<Alt+7>')
config.unbind('<Alt+8>')
config.unbind('<Alt+9>')
config.unbind('<Ctrl+w>')

# Help
config.bind('<Ctrl+h>', 'cmd-set-text -s :help')

# Increase scrolling amount
config.bind('j', 'scroll-px 0 200')
config.bind('k', 'scroll-px 0 -200')

config.bind('<Ctrl+p>', 'open -p')

# Bindings for insert mode
config.bind('<Ctrl+a>', 'fake-key <Home>', mode='insert')
config.bind('<Ctrl+d>', 'fake-key <Delete>', mode='insert')
config.bind('<Ctrl+e>', 'fake-key <End>', mode='insert')
config.bind('<Ctrl+f>', 'fake-key <Right>', mode='insert')
config.bind('<Ctrl+h>', 'fake-key <Backspace>', mode='insert')
config.bind('<Ctrl+w>', 'fake-key <Alt-backspace>', mode='insert')
config.bind('<Ctrl+y>', 'insert-text {primary}', mode='insert')
