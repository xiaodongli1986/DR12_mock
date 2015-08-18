

module tools
use ap_tools
	
	implicit none
	
	contains
	
! #################################################
! Let a series of ra,dec coordinates passing the masks
! # radecfile: the file whose first, second column is ra, dec; with a # of num_data
! # mangledir: location of the polyid EXE
! # SGmaskfile: the Survey Geometry mask file
! # vetomaskfilelist: a list of veto mask files; with a # of num_veto
! # SGrltfile, vetorltfile: name of files created as the result
! # do_mangle: whether run mangle?
! # SGmasked, vetomasked: two logical arrays with length num_data; 
!   telling us whether the point is rejected by SGmask/vetomask or not
!   ## it will be rejected if it is NOT lying on the SG mask, or lying on any of the veto masks
! # WeightinSG: weight of the point in SG mask file
! #################################################
	subroutine filt_data_by_masks(radecfile, num_data, mangledir, SGmaskfile, vetomaskfilelist, num_veto, &
		SGrltfile, vetorltfile, do_mangle, SGmasked, vetomasked, WeightinSG)
		! Dummy
		integer, intent(in) :: num_veto, num_data
		character(len=char_len), intent(in) :: radecfile, mangledir, SGmaskfile, vetomaskfilelist(num_veto), &
			SGrltfile, vetorltfile(num_veto)
		logical, intent(in) :: do_mangle
		logical, intent(out) :: SGmasked(num_data), vetomasked(num_veto,num_data) 
		real(dl), intent(out) :: WeightinSG(num_data)
		! Local
		character(len=char_len) :: cmdstr, tmpstr1, tmpstr2, tmpstr3
		integer :: i,j,k, n, i_veto, i_poly, vetofileunit(num_veto), tmpx1,tmpx2,tmpx3
		real(dl) :: SGwei(999999)

		! Count #-line of radecfile; check consistency
		call count_line_number(radecfile, n)
		if(n.ne.num_data) then
			print *, 'Error! #-line mismatches (radecfile): ', n, num_data; stop
		endif

		! Call mangle EXE and do something...
		if(do_mangle) then
			! command for SG mask
			cmdstr = trim(adjustl(mangledir))//' '//trim(adjustl(SGmaskfile))//' '//trim(adjustl(radecfile))&
				//' '//trim(adjustl(SGrltfile))
			print *, '#########################'
			write(*,'(5x,A)') 'Call Shell Command: '//trim(adjustl(cmdstr))
			print *, '#########################'
			call system(cmdstr)
			! commands for veto masks
			do i = 1,num_veto	
				cmdstr = trim(adjustl(mangledir))//' '//trim(adjustl(vetomaskfilelist(i)))//' '//trim(adjustl(radecfile)) &
					//' '//trim(adjustl(vetorltfile(i)))
				print *, '#########################'
				write(*,'(5x,A)') 'Call Shell Command: '//trim(adjustl(cmdstr))
				print *, '#########################'
				call system(cmdstr)
			enddo	
		endif

	!############################################
	! Count #-lines of result files; check consistency
	! All file shall have #-of-line == num_data if 
	!  mangle works properly without any accident
	!############################################
		call count_line_number(SGrltfile, n)
		if(n.ne.num_data+1) then
			print *, 'Error! #-line mismatches (SG): ', n, num_data+1; stop
		endif
		do i_veto = 1, num_veto
		       	call count_line_number(vetorltfile(i_veto),n)
			if(n.ne.num_data+1) then
				print *, 'Error! #-line mismatches (veto): ', i_veto, n, num_data+1; stop
			endif
		enddo

	!#################################
	! Read in the SG maske file
	! To get the weight (SGwei)
	!################################
		open(unit=2,file=SGmaskfile)
		i = 0
		do while(.true.)
			read(2,'(A)',end=3255) tmpstr1
			if(tmpstr1(1:7).ne.'polygon') cycle
			i = i+1
			if(i>999999) then
				print *, 'Overflow in SGwei! Increase your array size!'; stop
			endif
			! This redas in the weight of the polygin id; 
			read(tmpstr1(52:62),*) SGwei(i)
			if(SGwei(i).ne.0.0 .and. SGwei(i).lt.0.7_dl) then
				print *, 'Warning! Find polygon weights < 0.7 and non zero:', &
				  i, SGwei(i), '; programe will not stop';
				!stop
			endif
			read(tmpstr1(25:30),*) i_poly
			if(i_poly .ne. i-1) then
				print *, 'Error! Poly ID mismatches: ', i_poly, i-1; stop
			endif
			cycle
3255			exit
		enddo
		print *, '# of polygon = ', i; 
		close(2)
	
	!############################################
	! SGmasked, WeightinSG
	!############################################	
	
		SGmasked = .true.; WeightinSG = 0.0 ! this initialize the whole array.
		open(unit=2,file=SGrltfile)
		read(2,*) tmpstr1 ! first line is comment
		i = 0; j =0
		do while(.true.)
		        read(2,'(A)',end=103) tmpstr1
		        i = i+1
		        read(tmpstr1,*,end=102) tmpx1, tmpx2, tmpx3 ! tmpx3 is the polygin id
			! if positive weight, then this point is not masked
		        if(SGwei(int(tmpx3+0.5+1)).gt.1.0e-5_dl) then
		        	SGmasked(i) = .false.; j = j+1
				WeightinSG(i) = SGwei(int(tmpx3+0.5+1))
		        endif
102			cycle !print *, 'End when read in from str = ', trim(adjustl(tmpstr1))
103	             exit
		enddo
		write(*,'(A,i8,f8.4)') '#/rat of left (mask='//trim(adjustl(SGmaskfile))//'):', j, dble(j)/dble(num_data)
		close(2);
		
		! add the weight as the last column to the SGrltfile!!!
		print *, 'Adding weight to the last column of the SGrltfile...'
		print *, 'Temporary file TmpFileForWeight.txt...'
		open(unit=9876,file='TmpFileForWeight.txt')
		write(9876,*) 'polygin-weight'
		do i = 1, num_data
			write(9876,*) WeightinSG(i)
		enddo
		close(9876)
		
		cmdstr = 'pr -mts" " '//trim(adjustl(SGrltfile)) // &
			' TmpFileForWeight.txt >> '//trim(adjustl(SGrltfile))//'.WeiAdded' 
		print *, '#########################'
		write(*,'(5x,A)') 'Call Shell Command: '//trim(adjustl(cmdstr))
		print *, '#########################'
		call system(cmdstr)
		call system('rm TmpFileForWeight.txt')
		



	!############################################
	! vetomasked
	!############################################

		vetomasked = .false. ! this initialize a 2d array
		do i_veto = 1, num_veto
		        vetofileunit(i_veto) = 8927+i_veto 
		        open(unit=vetofileunit(i_veto),file=vetorltfile(i_veto))
		        read(vetofileunit(i_veto),*) tmpstr1
		        j=0; i=0;
		        do while(.true.)
		        	read(vetofileunit(i_veto),'(A)',end=105) tmpstr1
		        	i = i+1
		        	read(tmpstr1,*,end=104) tmpx1, tmpx2, tmpx3
		        	vetomasked(i_veto,i) = .true.; j=j+1
104				cycle
105				exit
			enddo
			write(*,'(A,i8,f8.4)') '#/rat of rejected (mask='//trim(adjustl(vetomaskfilelist(i_veto)))//'):',&
				j, dble(j)/dble(num_data)
		enddo
		do i_veto=1,num_veto
			close(vetofileunit(i_veto))
		enddo
	end subroutine filt_data_by_masks
end module tools



program ap_main

use ap_tools
use tools
implicit none

        character(len=char_len) :: inifile, SGrltfile, vetorltfile(100), dataname, inputfile, datafile, &
                datafile_maskinfo_added, SGmaskfile, radecfile, cmdstr, tmpstr1, tmpstr2, tmpstr3, inputfilename, nowinputfile

	real(dl) :: x,y,z,vx,vy,vz,mass,red,ra,dec,radius, ramincut, ramaxcut, decmincut, decmaxcut, &
		tmpx1, tmpx2, tmpx3, SGwei(99999)
	integer :: i_mock, ivlos, nowpatch, iRSD, i_veto, i_poly, i,j,k,l, vetofileunit(100), num_data,n, num_polygon, SGw, vetow
	logical :: do_mangle
	logical, allocatable :: SGmasked(:), vetomasked(:,:)
	real(dl), allocatable :: WeightinSG(:), tmp(:,:)

	print *, 'Usage: EXE radecfile an-inifile-for-masks'
	print *, 'Output: radecfile'

	i = iargc()
	if(i .ne.2) then
		print *, 'Usage: EXE radecfile an-inifile-for-masks'
		stop
	endif
	call getarg(1, radecfile)
	call getarg(2, inifile)

	print *

	!###################################
	! File names
	
        ! Names of mask files are stored as part of the ini file, 
	!  and read in through subroutine "get_info_from_ini" in the ap_tools.f90;
	call get_info_from_ini(inifile)

	! Names of output files:
	!  just adding some suffex to the input files 
	SGrltfile = trim(adjustl(radecfile))//'.SurveyGeometryMaskrlt'
	do i = 1, num_veto
		write(tmpstr1, *) i
		vetorltfile(i) = trim(adjustl(radecfile))//'.vetorlt'//trim(adjustl(tmpstr1))
	enddo
	
	!###################################
	! allocate arrays...
	call count_line_number(radecfile, num_data)
	if(allocated(tmp)) deallocate(tmp)
	if(allocated(SGmasked)) deallocate(SGmasked)
	if(allocated(vetomasked)) deallocate(vetomasked)
	allocate(SGmasked(num_data),vetomasked(num_veto,num_data),WeightinSG(num_data))
	
	
	print *, 'Filt data by masks:'
	write(*,'(A,A)')  '  radecfile  = ', trim(adjustl(radecfile))
	SGmaskfile = Andersonmaskfile
	write(*,'(A,A)')  '  basic mask = ', trim(adjustl(SGmaskfile))
	write(*,'(A,A)')  '  vetomask   = ', trim(adjustl(vetomaskfilelist(1)))
	do i = 2, num_veto
			write(*,'(A)'), '                ', trim(adjustl(vetomaskfilelist(i)))
	enddo
	
	!###################################
	! Passing the radec file through 
	!  masks; Generating a series of result files
	!  and some arrays;
	do_mangle = .true.
	call filt_data_by_masks(radecfile, num_data, mangledir, SGmaskfile, vetomaskfilelist(1:num_veto), num_veto, &
		SGrltfile, vetorltfile(1:num_veto), do_mangle, SGmasked, vetomasked, WeightinSG)

	!###################################
	! Report the final result.
        j=0;k=0;l=0 !  rejected by SG; rejected by veto; not rejrected
	do i = 1, num_data
		if(SGmasked(i)) then
			SGw = 0; j=j+1
		else
			SGw = 1
		endif
		vetow = 1
		do i_veto=1,num_veto
			if(vetomasked(i_veto,i).eq..true.) then
				vetow = 0; k = k+1; exit
			endif
		enddo
                if(SGw*vetow .eq. 1) then
                        l = l+1
                endif
	enddo
	print * 
	write(*,'(A,4i8)') '##### Final Result: tot / rejected-by-SG / rejected-by-veto / not-rejected-by-all-masks = ', num_data,j,k,l


!	call system('rm -rf '//trim(adjustl(SGrltfile)))
!	call system('rm -rf '//trim(adjustl(SGrltfile))//'.WeiAdded')
!	do i_veto = 1, num_veto
!		call system('rm -rf '//trim(adjustl(Vetorltfile(i_veto))))
!	enddo
!	r = de_get_comovr(dble(red))
!	call radecr_to_xyz(ra2,dec2,r,x,y,z)
        print *
end program ap_main
