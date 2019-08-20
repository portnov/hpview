#!/bin/bash
set -e

curl -sSL https://github.com/portnov/hpview/archive/master.zip -o master.zip
unzip master.zip && rm master.zip
cd hpview-master/
stack install --work-dir=./build --allow-different-user

cp /root/.local/bin/hpview /dst/
