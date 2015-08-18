
! 2014-June-10: output format of #-line corrected


program ap_main

use ap_tools
implicit none

        character(len=char_len) :: inifile,  dataname, datafile, inputfilename, nowinputfile, &
                datafile_info_added, radecfile, radecfile2, radecfiberfile, cmdstr, tmpstr1, tmpstr2, tmpstr3, tmpstr4

	real(dl) :: x,y,z,vx,vy,vz,mass,red,ra,dec,radius, ramincut, ramaxcut, decmincut, decmaxcut, tmpx1, tmpx2, tmpx3, 	sum_wcp0, sum_wcplarge, rat_wcp
	integer :: i_mock, nowpatch, iRSD, ivlos, i_veto, i_poly, i,j,k,l, num_data, num_radec, num_acpt, icount
	real(dl), allocatable :: tmp(:,:), tmpradec(:,:), tmpfiber(:,:)

	i = iargc()
	if(i .ne.1) then
		print *, 'Usage: EXE inifile'
		stop
	endif
	call getarg(1, inifile)
	call get_info_from_ini(inifile)
	print *

	icount = 0
    do i_mock = imock0, imock1
        print *, '##########################################'
        print *, ' i_mock = ', i_mock

    do nowpatch = 1, num_patch
    do iRSD = 1,2
    do ivlos = 0, num_vlos
	
	icount = icount + 1

	! i_mock, i_patch and names
	inputfilename = get_inputfilename(i_mock)                
        write(tmpstr1,*) nowpatch
        nowinputfile =  trim(adjustl(outputdir))//trim(adjustl(patchname))//'/'//trim(adjustl(inputfilename))//'.patch'//trim(adjustl(tmpstr1)) 

	! iRSD and names
        if(iRSD.eq.1) then
        	if(ivlos.ne.0) cycle
		if(only_do_vlosadd) cycle
                datafile = trim(adjustl(nowinputfile))//'.noRSD-radial-selected'
        elseif(ivlos.eq.0) then
		if(only_do_vlosadd) cycle
                datafile = trim(adjustl(nowinputfile))//'.RSD-radial-selected'
        else
        	write(tmpstr1, *) int(vlosadds(ivlos)+0.5)
		datafile = trim(adjustl(nowinputfile))//'.RSD-radial-selected.vlos'//trim(adjustl(tmpstr1))
        endif

	! radec, fiber file names
        radecfile = trim(adjustl(datafile))//'.radec'
        radecfile2 = trim(adjustl(datafile))//'.radec2'
        radecfiberfile = trim(adjustl(radecfile))//'.fiberinfo'
        
        ! output file names
	datafile_info_added = trim(adjustl(datafile)) ! write to the same file!!!
        write( *, '(A)')  '  datafile (input):  ', trim(adjustl(datafile))
        write( *, '(A)')  '           (output): ', trim(adjustl(datafile_info_added))

	! read in the main data file
	if(allocated(tmp)) deallocate(tmp)
	call read_in(datafile, 11, num_data, tmp, skipfirst_ipt = 0)

	! radecfile2 only contains ra/dec of acpted galaxies; will do fiber collision for them...
	num_acpt = 0
	open(unit=239,file=radecfile2)
	do i = 1, num_data
		if(tmp(i,10)>0.5.and.tmp(i,11)>0.5) then ! column 7, 8 are Anderwei, Vetowei
			call xyz_to_radecr(tmp(i,1),tmp(i,2),tmp(i,3),ra,dec,radius)
			write(239,'(2e15.7)') ra,dec
			num_acpt = num_acpt+1
		endif
	enddo
	close(239)

	! do fiber collision;
	print *, '###  Fiber Collision (Begin) ###############'
        cmdstr = '~/software/add_fiber/add_fiber -gal '//trim(adjustl(radecfile2))//' -out '//trim(adjustl(radecfiberfile)) &
        	//' -rmax 0.01722222222'
	call system(cmdstr)

	! read in the result; check the line number
	if(allocated(tmpfiber)) deallocate(tmpfiber)
	call read_in(radecfiberfile, 3, num_radec, tmpfiber, skipfirst_ipt = 0)
	if(num_radec.ne.num_acpt) then
		print *, 'ERROR (creat-HR3-mock-add_info): Mismatched numbers (radecfiberfile,radecfile2): ', num_radec, num_acpt
		stop
	endif
	
	! remove redundant files
	cmdstr = 'rm -rf '//trim(adjustl(radecfiberfile))//' '//trim(adjustl(radecfile2))
	call system(cmdstr)
	
	! check number/ratio of cp
	sum_wcp0=0.0; sum_wcplarge=0.0
	do i = 1, num_radec
		if(tmpfiber(i,3).lt.0.5) then
			sum_wcp0=sum_wcp0+1
		elseif(tmpfiber(i,3).gt.1.5) then
			sum_wcplarge=sum_wcplarge+tmpfiber(i,3)-1.0
		endif
	enddo
	rat_wcp=sum_wcp0/dble(num_radec)
	write(*,'(A,2i8,A,f10.3,A)') 'Find Close-Pair: ',int(sum_wcp0+0.5),&
		int(sum_wcplarge+0.5),'(', real(rat_wcp)*100.0, '%)'
	print *, '###  Fiber Collision (End) #################'

!############################################
! output result
!############################################   
	if(.true.) then
		open(unit=1,file=datafile_info_added)
		j = 0
		do i = 1, num_data
			if(tmp(i,10)<0.5.or.tmp(i,11)<0.5) then
			    write(1,'(5e15.7,i10,3i3,3e15.7)') tmp(i,1:3), tmp(i,7:8), int(tmp(i,9)+0.5),int(tmp(i,10)+0.5),int(tmp(i,11)+0.5), 1, tmp(i,4:6) !x,y,z, weight [~1/nbar],mass, ID,AndersonWeight,VetoWeight,CPWeight, vx,vy,vz
			else
			    j = j+1
			    write(1,'(5e15.7,i10,3i3,3e15.7)') tmp(i,1:3), tmp(i,7:8), int(tmp(i,9)+0.5),int(tmp(i,10)+0.5),int(tmp(i,11)+0.5), int(tmpfiber(j,3)+0.5), tmp(i,4:6)
			endif
		enddo
		close(1)

		! consistency check: fiber collision
		if(j.ne.num_acpt) then
			print *, 'ERROR (creat-HR3-mock-add_info): Mismatched numbers (j,num_acpt): ', j, num_acpt
			stop
		endif
	       	write(*,'(A)') ' rslt wrote to file  '//trim(adjustl(datafile_info_added))
	endif
	
!############################################
! calculating nbar and wfkp
!############################################
	write(tmpstr1,*) Area_Data
	print *
	print *, 'Calculating nbar & wfkp...'
	write(tmpstr3,*) redmin
	write(tmpstr4,*) redmax
	tmpstr2 = '~/software/calc_wfkp/calc_nbar -inputfilename '//trim(adjustl(datafile_info_added))//&
                    ' -zmin 0.0 -zmax 5.0  -zstep 0.005 -Area '//trim(adjustl(tmpstr1))//&
                    ' -hasweight True -col_weight 7 -hasweight2 True -col_weight2 8 '//&
                    ' -outputrlt False -appendrlt True -appendreplace True -skiprow 0'
        write(*,'(A,A)') '    cmd: ', trim(adjustl(tmpstr2))
	call system(tmpstr2)
        print *
    enddo
    enddo
    enddo
    enddo
end program ap_main
