#!/bin/bash

chmod a+x *.sh

for catname in LC93
do
	dirname=/home/xiaodongli/SparseFilaments/data/input/HR4/LC/
	filename=${catname}_massge3.5e12_rle1817
	echo $dirname, $filename

	scp -r xiaodongli@baekdu.kias.re.kr:$dirname$filename ./
	mv $filename ${filename}.00
done
