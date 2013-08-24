#!/usr/bin/env macruby
#
# Uses the scripting bridge to capture the current page in Chrome.
# Requires the Emacs server to be running.

framework 'ScriptingBridge'
require 'open-uri'
require 'rubygems'
require 'ruby_gntp'

def escape(str)
  URI.escape str, /[:?\/']/
end

# Get the URL and title of the current Chrome tab.
chrome = SBApplication.applicationWithBundleIdentifier 'com.google.Chrome'
tab = chrome.windows[0].activeTab
url = escape tab.URL
title = escape tab.title
# Get the current selected text.
prev_pb = `pbpaste`
tab.copySelection
body = escape `pbpaste` if `pbpaste` != prev_pb

# Call Emacs Client to capture.
template_key = 'l'
`emacsclient org-protocol:/capture:/#{template_key}/'#{url}'/'#{title}'/'#{body}'`

# Display Growl notification.
GNTP.notify({
  app_name: 'org-protocol',
  title: 'Link Captured',
  text: tab.title,
})
