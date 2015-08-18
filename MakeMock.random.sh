DR12v4-CMASS-N.random.ini DR12v4-CMASS-N
 ######################
 imock range =            0           9
 input  dir: ~/SparseFilaments/data/input/HR4/DR12_mock/
 output dir: ~/SparseFilaments/data/input/HR4/DR12_mock/
 inputfile: str1  = random.
 inputfile: str2  = .0vxvyvz.log10massfrom10to14.compact
 #-patch =            4
 patchname = DR12v4-CMASS-N
 ra min/max:     107.937130691000        264.907506672000     
 dec min/max:    -4.61931791229000        69.7376985415000     
 nbarfile = 
 ~/SparseFilaments/data/input/DR12/nbar/nbar-cmass-dr12v4-N-Reid.dat.ReCal.Inter
 poly
 #-rat: mock/data =    1.33716698243000     
 redmin/redmax =   0.420000000000000       0.710000000000000     
 ~/software/mangle8-2.2/bin/polyid
 ~/SparseFilaments/data/input/DR12/mask/mask-cmass-dr12v4-N-Reid.ply
 #-veto mask:           6
 ~/SparseFilaments/data/input/DR12/newmasks/allsky_bright_star_mask_pix.ply
 ~/SparseFilaments/data/input/DR12/newmasks/badfield_mask_postprocess_pixs8.ply.
 snap
 ~/SparseFilaments/data/input/DR12/newmasks/badfield_mask_unphot_seeing_extincti
 on_pixs8_dr12.ply
 ~/SparseFilaments/data/input/DR12/newmasks/centerpost_mask_dr12.ply
 ~/SparseFilaments/data/input/DR12/newmasks/collision_priority_mask_dr12.ply
 ~/SparseFilaments/data/input/DR12/newmasks/bright_object_mask_rykoff_pix.ply
 Only do vlos add:  F
 Effective Area of Data:    6851.41900000000     
 Extra_radrop,  F
 
 inputfile: ~/SparseFilaments/data/input/HR4/DR12_mock/random.00.0vxvyvz.log10massfrom10to14.compact
 outputfile: ~/SparseFilaments/data/input/HR4/DR12_mock/DR12v4-CMASS-N/random.00.0vxvyvz.log10massfrom10to14.compact.patch1
 outputfile: ~/SparseFilaments/data/input/HR4/DR12_mock/DR12v4-CMASS-N/random.00.0vxvyvz.log10massfrom10to14.compact.patch2
 outputfile: ~/SparseFilaments/data/input/HR4/DR12_mock/DR12v4-CMASS-N/random.00.0vxvyvz.log10massfrom10to14.compact.patch3
 outputfile: ~/SparseFilaments/data/input/HR4/DR12_mock/DR12v4-CMASS-N/random.00.0vxvyvz.log10massfrom10to14.compact.patch4
 Rotation Matrix of patch (comparing it with python code for check)           1
      0.9758974      0.0000000E+00 -0.2182296    
      0.0000000E+00   1.000000      0.0000000E+00
      0.2182296      0.0000000E+00  0.9758974    
 Rotation Matrix of patch (comparing it with python code for check)           2
     -0.9758974      1.1951297E-16 -0.2182296    
     -1.2246469E-16  -1.000000      0.0000000E+00
     -0.2182296      2.6725421E-17  0.9758974    
 Rotation Matrix of patch (comparing it with python code for check)           3
      0.1601043      0.0000000E+00  0.9871001    
      0.0000000E+00   1.000000      0.0000000E+00
     -0.9871001      0.0000000E+00  0.1601043    
 Rotation Matrix of patch (comparing it with python code for check)           4
     -0.1601043      1.9607124E-17  0.9871001    
     -1.2246469E-16  -1.000000      0.0000000E+00
      0.9871001     -1.2088490E-16  0.1601043    
 
 Inversed Rotation Matrix of patch (comparing it with python code for check)
           1
      0.9758974      0.0000000E+00  0.2182296    
      0.0000000E+00   1.000000      0.0000000E+00
     -0.2182296      0.0000000E+00  0.9758974    
 Inversed Rotation Matrix of patch (comparing it with python code for check)
           2
     -0.9758974     -1.2246469E-16 -0.2182296    
      1.1951297E-16  -1.000000      2.6725421E-17
     -0.2182296      0.0000000E+00  0.9758974    
 Inversed Rotation Matrix of patch (comparing it with python code for check)
           3
      0.1601043      0.0000000E+00 -0.9871001    
      0.0000000E+00   1.000000      0.0000000E+00
      0.9871001      0.0000000E+00  0.1601043    
 Inversed Rotation Matrix of patch (comparing it with python code for check)
           4
     -0.1601043     -1.2246469E-16  0.9871001    
      1.9607124E-17  -1.000000     -1.2088490E-16
      0.9871001      0.0000000E+00  0.1601043    
forrtl: error (69): process interrupted (SIGINT)
Image              PC                Routine            Line        Source             
libpthread.so.0    0000003153E0E4D0  Unknown               Unknown  Unknown
creat-HR3-mock-cr  00000000004255E6  Unknown               Unknown  Unknown
creat-HR3-mock-cr  000000000042666B  Unknown               Unknown  Unknown
creat-HR3-mock-cr  00000000004415C8  Unknown               Unknown  Unknown
creat-HR3-mock-cr  0000000000414208  Unknown               Unknown  Unknown
creat-HR3-mock-cr  00000000004031CC  Unknown               Unknown  Unknown
libc.so.6          000000315321ECDD  Unknown               Unknown  Unknown
creat-HR3-mock-cr  00000000004030D9  Unknown               Unknown  Unknown
