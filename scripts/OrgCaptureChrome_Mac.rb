#!/usr/bin/env macruby
#
# Uses the scripting bridge to capture the current page in Chrome.
# Requires the Emacs server to be running.

framework 'ScriptingBridge'
require 'open-uri'

# Get the URL and title of the current Chrome tab.
chrome = SBApplication.applicationWithBundleIdentifier 'com.google.Chrome'
tab = chrome.windows[0].activeTab
title = URI::encode tab.title
url = URI::encode tab.URL
# Call Emacs Client to capture.
`emacsclient org-protocol:/store-link:/#{url}/#{title}`
