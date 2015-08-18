#!/bin/bash

#for catname in DR12v4-CMASS-N # DR12v4-CMASS-S DR12v4-LOWZ-N DR12v4-LOWZ-S
#for catname in DR12v4-CMASS-N DR12v4-CMASS-S DR12v4-LOWZ-N DR12v4-LOWZ-S # # 
for catname in DR12v4-CMASS-S

do 
	for catname2 in B08 # M12 V13 B08 J08 # J08Slice.z_0.5 J08Slice.z_0 random HR3 LC93 #random #HR3 #LC93
	do
		inputini=$catname.$catname2.ini
		catdir=$catname
		echo $inputini $catdir
		./creat-HR3-mock-creat_patched_mocks $inputini
		./creat-HR3-mock-radial_select $inputini
		./creat-HR3-mock-add_mask $inputini
		./creat-HR3-mock-add_info $inputini
		rm $catdir/*patch?
	done
done
