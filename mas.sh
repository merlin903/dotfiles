#!/usr/bin/env bash

if [ -x "$(command -v brew)" ]; then
    if ! [ -x "$(command -v mas)" ]; then
	brew install mas
    fi
fi

if [ -x "$(command -v mas)" ]; then
    mas install 1289583905 # Pixelmator Pro (1.5.5)
    mas install 409203825  # Numbers (6.2.1)
    mas install 441258766  # Magnet (2.4.5)
    mas install 409183694  # Keynote (9.2.1)
    mas install 467939042  # Growl (2.1.3)
    mas install 497799835  # Xcode (11.3.1)
    mas install 1014850245 # Monit (2.0.1)
    mas install 1376402589 # StopTheMadness (11.1)
    mas install 924726344  # Deliveries (3.2.2)
    mas install 1384080005 # Tweetbot (3.3.3)
    mas install 409201541  # Pages (8.2.1)
    mas install 1107421413 # 1Blocker (3.2)
    mas install 1449412482 # Reeder (4.2.3)
    mas install 475260933  # HardwareGrowler (2.2)
    mas install 970502923  # Typeeto (1.5)
    mas install 1262957439 # Textual IRC Client (7.1.5)
    mas install 1496833156 # Playgrounds (3.2)
    mas install 1176895641 # Spark (2.5.5)
    mas install 897118787  # Shazam (2.10.0)
fi

export MONIT_HELPER_BASE="https://mmonit.com/widget/"
export MONIT_HELPER_FILE=`curl $MONIT_HELPER_BASE | egrep -o '[-A-Za-z0-9.]+\.zip'`

curl -o $MONIT_HELPER_FILE "$MONIT_HELPER_BASE/download/$MONIT_HELPER_FILE"
unzip -d monit $MONIT_HELPER_FILE
open "monit/Monit Helper Installer.app"
read -p "Press enter to clean up after ourselves"
rm -rf monit
