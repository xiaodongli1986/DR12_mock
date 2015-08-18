#!/bin/bash

for catname in J08 M12 V13 B08 HR4PSB # LC93 
do
	for index in 00
	do
		#ls ${catname}_massge3.5e12_rle1817.$index -alh
		#head ${catname}_massge3.5e12_rle1817.$index 
		LSS_MakeCompactSample -input ${catname}_massge3.5e12_rle1817.$index -omegam 0.26 -w -1.0 -xcol 1 -ycol 2 -zcol 3 -vxcol 4 -vycol 5 -vzcol 6 -masscol 7
	done
done
