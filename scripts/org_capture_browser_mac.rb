#!/usr/bin/env macruby
#
# Uses the scripting bridge to capture the current page in Safari.
# Requires the Emacs server to be running.

framework 'ScriptingBridge'
require 'uri'

# Bundle IDs
SAFARI = 'com.apple.Safari'
CHROME = 'com.google.Chrome'

# Find the current open browser by searching the process list.
def running_browser
  procs = `ps -ef`.split '\n'
  [SAFARI, CHROME].first { |id| procs =~ id  }
end

# Get the title and URL of the current browser.
def current_browser_tab(bundleId)
  case bundleId
  when SAFARI
    app = SBApplication.applicationWithBundleIdentifier 'com.apple.Safari'
    tab = app.windows[0].currentTab
    [tab.URL, tab.name]
  when CHROME
    app = SBApplication.applicationWithBundleIdentifier 'com.google.Chrome'
    tab = app.windows[0].activeTab
    [tab.URL, tab.title]
  end
end

# Escape the given string passing to Emacs.
def escape(str)
  URI.escape str, /[:?\/']/
end

# Capture URL and TITLE with emacsclient.
def capture(url, title)
  template_key = 'l'
  ec = '/usr/local/bin/emacsclient'
  `#{ec} org-protocol:/capture:/#{template_key}/'#{escape url}'/'#{escape title}'/''`
end

# Perform capture.
app = running_browser
url, title = current_browser_tab app
capture url, title
