#!/bin/bash

#LSS_HR4LC_SliceSelection
#LSS_MakeCompactSample -input J08.dat.z_0.mge8e12.00 -omegam 0.26 -w -1.0 -xcol 1 -ycol 2 -zcol 3 -vxcol 4 -vycol 5 -vzcol 6 -masscol 7
#LSS_rmass_stat -input J08.dat.z_0.mge8e12.00 -rmin 0 -rmax 1800.0 -nummassbin 1 -numrbin 60 -xcol 1 -ycol 2 -zcol 3 -masscol 7

LSS_HR4LC_SliceSelection J08.dat.z_0
LSS_HR4LC_SliceSelection J08.dat.z_0.5
LSS_MakeCompactSample -input J08.dat.z_0.5.mge4.05e12.00 -omegam 0.26 -w -1.0 -xcol 1 -ycol 2 -zcol 3 -vxcol 4 -vycol 5 -vzcol 6 -masscol 7
LSS_rmass_stat -input J08.dat.z_0.5.mge4.05e12.00 -rmin 0 -rmax 1800.0 -nummassbin 1 -numrbin 60 -xcol 1 -ycol 2 -zcol 3 -masscol 7
