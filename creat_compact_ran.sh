#!/bin/bash
for index in 0 1 2 3 4 5 6 7 8 9
do
	LSS_MakeCompactSample -input random.0$index.0vxvyvz.log10massfrom10to14 -omegam 0.26 -w -1.0 -xcol 1 -ycol 2 -zcol 3 -vxcol 4 -vycol 5 -vzcol 6 -masscol 7
done
