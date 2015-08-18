#!/bin/bash

read -p "This will remove all DR12v4 mock files!!! Are you really sure? " -n 1 -r
echo    # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]
then
	for catname in DR12v4-CMASS-N DR12v4-CMASS-S DR12v4-LOWZ-N DR12v4-LOWZ-S
	do
	#	rm $catname/*.* 
	done
fi

