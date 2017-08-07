#!/bin/bash

# Install `mas` if not already isntalled
brew install mas

# Open and verify signed-in
open -a "/Applications/App Store.app"
read -p "Ensure you are signed in to the App Store, then hit enter."

# Install apps
mas install 1107421413  # 1Blocker (1.3.1)
mas install 1176895641  # Spark (1.2.3)
mas install 407963104   # Pixelmator (3.6)
mas install 409183694   # Keynote (7.2)
mas install 409201541   # Pages (6.2)
mas install 409203825   # Numbers (4.2)
mas install 409789998   # Twitter (4.3.2)
mas install 443987910   # 1Password (6.8)
mas install 467939042   # Growl (2.1.3)
mas install 468707058   # TuneUp (2.12)
mas install 475260933   # HardwareGrowler (2.2)
mas install 557168941   # Tweetbot (2.5.1)
mas install 792425898   # Flume (2.4.2)
mas install 880001334   # Reeder (3.0.5)
mas install 884952790   # iTranslate (1.4.3)
mas install 896450579   # Textual (6.0.10)
mas install 897118787   # Shazam (1.2.3)
mas install 924726344   # Deliveries (3.0.3)
mas install 927292435   # iStat Mini (1.1)
mas install 970502923   # Typeeto (1.4.3)
