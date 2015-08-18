#!/bin/bash

for catname in DR12v4-CMASS-S DR12v4-LOWZ-N DR12v4-LOWZ-S # DR12v4-CMASS-N 
do

	bigfile=$catname/mockran
	rm $bigfile
#	#ls $catname/*ran*.noRSD*selected -alh
	cat $catname/ran*patch1*.noRSD*selected $catname/ran*patch2*.noRSD*selected  $catname/ran*patch3*.noRSD*selected  $catname/ran*patch4*.noRSD*selected  >> $bigfile
#	cat $catname/*ran*.noRSD*selected >> $bigfile


#	bigfile2=$catname/mockran_patchy
#	rm $bigfile2
#	cat $catname/ran*patch1 $catname/ran*patch2 $catname/ran*patch3 $catname/ran*patch4 >> $bigfile2
done

