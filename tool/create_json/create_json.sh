#!/bin/bash

P=8
TARDIR="~/code/mhhss_all/inputs/dispatch/strong/dispatch_12_$P"
mkdir $TARDIR -p
~/code/mhhss_all/tool/create_json/ctdp -d 12 -p $P -o $TARDIR/dispatch.json

