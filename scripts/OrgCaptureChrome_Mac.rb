#!/usr/bin/env macruby
#
# Uses the scripting bridge to capture the current page in Chrome.
# Requires the Emacs server to be running.

framework 'ScriptingBridge'
require 'open-uri'

def escape(str)
  URI.escape str, /[:?\/']/
end

# Get the current Chrome tab.
chrome = SBApplication.applicationWithBundleIdentifier 'com.google.Chrome'
tab = chrome.windows[0].activeTab
# Get fields to capture.
url = escape tab.URL
title = escape tab.title
# Get the current selected text.
prev_pb = `pbpaste`
tab.copySelection
body = escape `pbpaste` if `pbpaste` != prev_pb
# Call Emacs Client to capture.
template_key = 'l'
`emacsclient org-protocol:/capture:/#{template_key}/'#{url}'/'#{title}'/'#{body}'`
