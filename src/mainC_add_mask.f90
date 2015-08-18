
! 2014-June-10: output format of #-line corrected


module tools
use ap_tools
	
	implicit none
	
	contains
	
	! #################################################
! subroutine which filt ra,dec data by masks
! #################################################
	subroutine filt_data_by_masks(radecfile, num_data, mangledir, Andersonmaskfile, vetomaskfilelist, num_veto, &
		Andersonrltfile, vetorltfile, do_mangle, Andersonmasked, vetomasked)
		! Dummy
		integer, intent(in) :: num_veto, num_data
		character(len=char_len), intent(in) :: radecfile, mangledir, Andersonmaskfile, vetomaskfilelist(num_veto), &
			Andersonrltfile, vetorltfile(num_veto)
		logical, intent(in) :: do_mangle
		logical, intent(out) :: Andersonmasked(num_data), vetomasked(num_veto,num_data)
		! Local
		character(len=char_len) :: cmdstr, tmpstr1, tmpstr2, tmpstr3
		integer :: i,j,k, n, i_veto, i_poly, vetofileunit(num_veto), tmpx1,tmpx2,tmpx3
		real(dl) :: Andersonwei(999999)

		call count_line_number(radecfile, n)
		if(n.ne.num_data) then
			print *, 'Error! #-line mismatches (radecfile): ', n, num_data; stop
		endif

		if(do_mangle) then
			cmdstr = trim(adjustl(mangledir))//' '//trim(adjustl(Andersonmaskfile))//' '//trim(adjustl(radecfile))&
				//' '//trim(adjustl(Andersonrltfile))
			write(*,'(5x,A)') 'Command: '//trim(adjustl(cmdstr))
			call system(cmdstr)
			do i = 1,num_veto	
				cmdstr = trim(adjustl(mangledir))//' '//trim(adjustl(vetomaskfilelist(i)))//' '//trim(adjustl(radecfile)) &
					//' '//trim(adjustl(vetorltfile(i)))
				write(*,'(5x,A)') 'Command: '//trim(adjustl(cmdstr))
				call system(cmdstr)
			enddo	
		endif

	!############################################
	! check #-line ...
	!############################################
		call count_line_number(Andersonrltfile, n)
		if(n.ne.num_data+1) then
			print *, 'Error! #-line mismatches (Anderson): ', n, num_data+1; stop
		endif
		do i_veto = 1, num_veto
		       	call count_line_number(vetorltfile(i_veto),n)
			if(n.ne.num_data+1) then
				print *, 'Error! #-line mismatches (veto): ', i_veto, n, num_data+1; stop
			endif
		enddo

	!#################################
	! read-in the Anderson maske file
	!################################
		open(unit=2,file=Andersonmaskfile)
		i = 0
		do while(.true.)
			read(2,'(A)',end=3255) tmpstr1
			if(tmpstr1(1:7).ne.'polygon') cycle
			i = i+1
			if(i>999999) then
				print *, 'Overflow in Andersonwei! Increase your array size!'; stop
			endif
			read(tmpstr1(52:62),*) Andersonwei(i)
			if(Andersonwei(i).ne.0.0 .and. Andersonwei(i).lt.0.7_dl) then
				print *, 'Warning! Find polygon weights < 0.7 and non zero:', &
				  i, Andersonwei(i), '; programe will not stop';
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
	! masked by Anderson
	!############################################	
	
		Andersonmasked = .true.
		open(unit=2,file=Andersonrltfile)
		read(2,*) tmpstr1 ! first line is comment
		i = 0; j =0
		do while(.true.)
		        read(2,'(A)',end=103) tmpstr1
		        i = i+1
		        read(tmpstr1,*,end=102) tmpx1, tmpx2, tmpx3
		        if(Andersonwei(int(tmpx3+0.5+1)).gt.0.69_dl) then
		        	Andersonmasked(i) = .false.; j = j+1
		        endif
102			cycle !print *, 'End when read in from str = ', trim(adjustl(tmpstr1))
103	             exit
		enddo
		write(*,'(A,i8,f8.4)') '#/rat of left (mask='//trim(adjustl(Andersonmaskfile))//'):', j, dble(j)/dble(num_data)
		close(2);

	!############################################
	! masked by veto
	!############################################

		vetomasked = .false.
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
			write(*,'(A,i8,f8.4)') '#/rat of dropped (mask='//trim(adjustl(vetomaskfilelist(i_veto)))//'):',&
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

        character(len=char_len) :: inifile, Andersonrltfile, vetorltfile(100), dataname, inputfile, datafile, &
                datafile_maskinfo_added, radecfile, cmdstr, tmpstr1, tmpstr2, tmpstr3, inputfilename, nowinputfile

	real(dl) :: x,y,z,vx,vy,vz,mass,red,ra,dec,radius, ramincut, ramaxcut, decmincut, decmaxcut, &
		tmpx1, tmpx2, tmpx3, Andersonwei(99999)
	integer :: i_mock, ivlos, nowpatch, iRSD, i_veto, i_poly, i,j,k,l, vetofileunit(100), num_data,n, num_polygon, Andersonw, vetow
	logical :: do_mangle
	logical, allocatable :: Andersonmasked(:), vetomasked(:,:)
	real(dl), allocatable :: tmp(:,:)

	i = iargc()
	if(i .ne.1) then
		print *, 'Usage: EXE inifile'
		stop
	endif
	call getarg(1, inifile)
	call get_info_from_ini(inifile)
	print *

    do i_mock = imock0, imock1
        print *, '##########################################'
        print *, ' i_mock = ', i_mock

    do nowpatch = 1, num_patch
    do iRSD = 1,2
    do ivlos = 0, num_vlos

	inputfilename = get_inputfilename(i_mock)                

        write(tmpstr1,*) nowpatch
        nowinputfile =  trim(adjustl(outputdir))//trim(adjustl(patchname))//'/'//trim(adjustl(inputfilename))//'.patch'//trim(adjustl(tmpstr1)) 
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
        radecfile = trim(adjustl(datafile))//'.radec'

	! file names
	datafile_maskinfo_added = trim(adjustl(datafile)) ! write to the same file!!!
        write( *, '(A)')  '  datafile (input,output the same file): ', trim(adjustl(datafile_maskinfo_added))
        
	Andersonrltfile = trim(adjustl(datafile))//'.Andersonrlt_mock'//trim(adjustl(tmpstr1))//'.txt'
	do i_veto = 1, num_veto
		write(tmpstr2,*) i_veto
		vetorltfile(i_veto) = trim(adjustl(datafile))//'.vetorlt_mock'//trim(adjustl(tmpstr1))//'_'//trim(adjustl(tmpstr2))//'.txt'
	enddo
	
	call count_line_number(radecfile, num_data)
	if(allocated(tmp)) deallocate(tmp)
	call read_in(datafile, 9, n, tmp)
	if(n.ne.num_data) then
		print *, 'Error ! #-lines mismatch: (data and radec)', n, num_data; stop
	endif
		
	if(allocated(Andersonmasked)) deallocate(Andersonmasked)
	if(allocated(vetomasked)) deallocate(vetomasked)
	allocate(Andersonmasked(num_data),vetomasked(num_veto,num_data))
	
	do_mangle = .true.
	
	print *, 'Filt data by masks:'
	write(*,'(A,A)')  '  radecfile  = ', trim(adjustl(radecfile))
	write(*,'(A,A)')  '  basic mask = ', trim(adjustl(Andersonmaskfile))
	write(*,'(A,A)')  '  vetomask   = ', trim(adjustl(vetomaskfilelist(1)))
	do i = 2, num_veto
			write(*,'(A)'), '                ', trim(adjustl(vetomaskfilelist(i)))
	enddo
	
	call filt_data_by_masks(radecfile, num_data, mangledir, Andersonmaskfile, vetomaskfilelist(1:num_veto), num_veto, &
		Andersonrltfile, vetorltfile(1:num_veto), do_mangle, Andersonmasked, vetomasked)

!############################################
! output result
!############################################    
        j=0;k=0;l=0
	open(unit=1,file=datafile_maskinfo_added)
	do i = 1, num_data
		if(Andersonmasked(i)) then
			Andersonw = 0; j=j+1
		else
			Andersonw = 1
		endif
		vetow = 1
		do i_veto=1,num_veto
			if(vetomasked(i_veto,i).eq..true.) then
				vetow = 0; k = k+1; exit
			endif
		enddo
                write(1,'(8e15.7,i10,2i3)') tmp(i,1:8), int(tmp(i,9)+0.5), Andersonw, vetow
                if(Andersonw*vetow .eq. 1) then
                        l = l+1
                endif
	enddo
	close(1)
	write(*,'(A,4i8)') '#: tot/masked-by-An/masked-by-veto/left = ', num_data,j,k,l
        write(*,'(A)') ' rslt wrote to file  '//trim(adjustl(datafile_maskinfo_added))

	call system('rm -rf '//trim(adjustl(Andersonrltfile)))
	do i_veto = 1, num_veto
		call system('rm -rf '//trim(adjustl(Vetorltfile(i_veto))))
	enddo
!	r = de_get_comovr(dble(red))
!	call radecr_to_xyz(ra2,dec2,r,x,y,z)
        print *
    enddo
    enddo
    enddo
    enddo
end program ap_main
