#!/bin/bash

for iran in 0 1 2 3 4 5 6 7 8 9
do
	ranfile=random.0$iran
	ranfile2=$ranfile.0vxvyvz.log10massfrom10to14
	## 1. Generate random positions
	### To have nbar = 1.0e-3 within r < 1808.4027526 we need 24,772,629 galaxies
#	LSS_make-allsky-random random.0 $iran 24773000  0 1808.4
	## 2. Add velocity, mass
#	LSS_add-0vxvyvz-randmass $ranfile 10 14
	##
	## 3. rm unnecessary files (we already have compact file)
	rm $ranfile $ranfile2
done
