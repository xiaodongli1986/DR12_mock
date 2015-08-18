!2014-July-10: add vlos mocks
!2014-June-10: special treatment at the high redshift boundary to correct suppression of nbar due to RSD

MODULE RDistributions
use ap_tools
! Inspired from: http://www.johndcook.com/julia_rng.html
! Original author in julia : John D Cook
! coded : Sukhbinder in fortran
! Date : 28th Feb 2012
!

!
! Non uniform random Number Generators in Fortran
!
      REAL(DL), PARAMETER :: PI=3.141592653589793238462
      CONTAINS

      FUNCTION rand_uniform(a,b) RESULT(c)
       REAL(DL) :: a,b,c,temp
       CALL RANDOM_NUMBER(temp)
       c= a+temp*(b-a)
      END FUNCTION

!
! Random Sample from normal (Gaussian) distribution
!
      FUNCTION rand_normal(mean,stdev) RESULT(c)
       REAL(DL) :: mean,stdev,c,temp(2)
      IF(stdev < 0.0d0) THEN
        WRITE(*,*) "Standard Deviation must be +ve"
      ELSE
        CALL RANDOM_NUMBER(temp)
        r=(-2.0d0*log(temp(1)))**0.5
        theta = 2.0d0*PI*temp(2)
        c= mean+stdev*r*sin(theta)
      END IF
      END FUNCTION rand_normal
end MODULE RDistributions    


program ap_main

!use mpi
use ap_cosmo_funs
use RDistributions

	implicit none

        integer*8, parameter :: bignum = 20500000
        integer :: numbins, np, IDs(bignum), nowpatch, numhalo, iRSD, ivlos, nowtotleftnum, &
                   markibin(bignum), numinbin, nownum, numgoal, prevnum, numA, numB, & !markibin records the bin number; 0 means not in bin
                   i,j, i_bin, i_mock, i_same
        real(dl) :: binnednum_data(1000), binnednum_target(1000), redmins(1000), redcenters(1000), redmaxs(1000), &
                 wfkp(1000), xs(bignum), ys(bignum),zs(bignum), zcosmos(bignum), zobss(bignum), rats(bignum), &
                 vxs(bignum), vys(bignum),vzs(bignum), vlos,zobs,rat,&
                 masss(bignum), zc,zl,zr, tmpx,tmpy,tmpz, totgoalnum, reds(bignum), massmin,massmax, red1, red2, r, &
                 massinbin(bignum), mass1,mass2,mass0,massA,massB, ABkeeprat,randx, ra,dec,radius, highdz,z1,z2,z3,r1,r2,r3,highztreat_rat 
        character(len=char_len) :: inifile, tmpstr, nowoutputfile, nowinputfile, &
        	radecoutputfile, masscutfile, tmpstr1, tmpstr2, tmpstr3, inputfilename
        real(dl), parameter ::  unitmass = 2.0*2.435*(10**11)
        logical :: treat_highz_bound = .true. !!! Settings

	i = iargc()
	if(i .ne.1) then
		print *, 'Usage: EXE inifile'
		stop
	endif
	call getarg(1, inifile)

	call get_info_from_ini(inifile)
	print *
! Initialize cosmology                
        gb_omegam = 0.26d0; gb_h = 0.72d0; gb_w = -1.0d0
        print *, '#########################'
        print *, 'Select out halos matching radial n(z) of data'
        print *, 'Cosmology: ', real(gb_omegam), real(gb_w)
        call cosmo_funs_init(.true.)
        call de_calc_comovr()

! Screen-print of basic information        
        print *
        print *, '#-patch = ', num_patch
        write(*,'(A,e14.7)') ' rat between mock/data = ', numrat
        write(*,'(A,A)') ' nbar file = ', trim(adjustl(nbarfile))
        print *

! Read in the information of nbar        
        print *, 'Read in info of nbar...'
        open(unit=1,file=nbarfile,action='read')
        read(1,*) tmpstr
        read(1,*) tmpstr
        numbins = 1
        do while(.true.)
                read(1,*,end=300) zc,zl,zr,tmpx,tmpx,tmpx,tmpx, tmpy,tmpz
                if(zc<redmin.or.zc>redmax) cycle
                binnednum_data(numbins) = tmpy*tmpz
                binnednum_target(numbins) = tmpy*tmpz*numrat
                redcenters(numbins) = zc; redmins(numbins) = zl; redmaxs(numbins) = zr;
                numbins = numbins+1
                cycle
300             exit                
        enddo
        numbins = numbins - 1; close(1)
        
        if(treat_highz_bound) then
        	highdz = 0.002
        	do i = 1, 2
			redmaxs(numbins) = redmaxs(numbins) - highdz
			redcenters(numbins) = redcenters(numbins) - highdz/2.0
			redmins(numbins+1) = redmaxs(numbins)
			redcenters(numbins+1) = redmins(numbins+1) + highdz/2.0
			redmaxs(numbins+1) = redmins(numbins+1) + highdz
			z1 = redmins(numbins); z2 = redmaxs(numbins); z3 = redmaxs(numbins+1)
			r1 = de_get_comovr(dble(z1)); r2 = de_get_comovr(dble(z2)); r3 = de_get_comovr(dble(z3));
			print *, 'z1,z2,z3, r1,r2,r3= ', z1,z2,z3, r1,r2,r3
			highztreat_rat = (r2**3.0-r1**3.0)/(r3**3.0-r1**3.0)
			binnednum_target(numbins+1) = binnednum_target(numbins)*(1.0-highztreat_rat)
			binnednum_target(numbins) = binnednum_target(numbins)*highztreat_rat
			binnednum_data(numbins+1) = binnednum_data(numbins)*(1.0-highztreat_rat)
			binnednum_data(numbins) = binnednum_data(numbins)*highztreat_rat
			print *, 'Special treatment of high z boundary (further split!!!)!!!: highdz/rat = ', &
				real(highdz), real(highztreat_rat)
			numbins = numbins + 1
			highdz = highdz/2.0
		enddo
        endif

!        print *, '#-bin: ', numbins
!        print *, ' i-bin  redmin, redmax, binnednum_data, binnednum_mock '
        do i = 1, numbins
                write(*,'(i3,3f10.5,2e14.7)') i, redcenters(i), redmins(i), redmaxs(i), binnednum_data(i), binnednum_target(i)
        enddo
        totgoalnum = sum(binnednum_target(1:numbins))

! Radially-select...        

    do i_mock = imock0, imock1
	inputfilename = get_inputfilename(i_mock) 
        write(*,'(A,A)') ' (basic) inputfile =', trim(adjustl(inputfilename))
    do nowpatch = 1, num_patch
         write(tmpstr1,*) nowpatch
         nowinputfile = trim(adjustl(outputdir))//trim(adjustl(patchname))//'/'//trim(adjustl(inputfilename))//'.patch'//trim(adjustl(tmpstr1)) ! Settings !!!
         print *
         numhalo = 1
         open(unit=1,file=nowinputfile)
         do while(.true.)
                read(1,*,end=301) xs(numhalo), ys(numhalo), zs(numhalo), &
                         vxs(numhalo), vys(numhalo), vzs(numhalo), masss(numhalo), zcosmos(numhalo),&
                         zobss(numhalo), rats(numhalo), IDs(numhalo)
                numhalo = numhalo + 1
                cycle
301             exit
        enddo
        numhalo = numhalo-1; 
        write(*,'(A,i2,A,A,A,i10)') '  Patch ', nowpatch, ': read in from file: ',  trim(adjustl(nowinputfile)), '; #=', numhalo
	close(1)

        np = numhalo
        do iRSD = 1, 2
        do ivlos = 0, num_vlos
                if(iRSD.eq.1) then
                	if(ivlos .ne. 0) cycle
                	if(only_do_vlosadd) cycle
                        print *, '  NO   RSD case:'
                else
                        if(only_do_vlosadd.and.ivlos.eq.0) cycle
                        print *, '  With RSD case:'
                endif
                if(iRSD.eq.1) then
                        reds(1:numhalo) = zcosmos(1:numhalo)
                        nowoutputfile = trim(adjustl(nowinputfile))//'.noRSD-radial-selected'
                elseif(ivlos.eq.0) then
                        reds(1:numhalo) = zobss(1:numhalo)
                        nowoutputfile = trim(adjustl(nowinputfile))//'.RSD-radial-selected'
		else
			print *, ' case ', ivlos, ': vlos = ', vlosadds(ivlos)
		        if(vlosadds(ivlos).lt. 0.0_dl) then
		        	print *, 'ERROR (mainZ_RSDMocks_for_Test):: vlos <0; skip; : ', ivlos, vlosadds
		        	cycle
		        endif
		        do i = 1, numhalo
		        	r = sqrt(xs(i)**2.0+ys(i)**2.0+zs(i)**2.0)
		        	vlos = (xs(i)*vxs(i)+ys(i)*vys(i)+zs(i)*vzs(i)) / r +  rand_normal(0.0_dl,vlosadds(ivlos))
		        	zobs = zcosmos(i) + vlos * (1.0+zcosmos(i)) / const_c
		        	rat = de_get_comovr(dble(zobs)) / r
				!robs = de_get_comovr(dble(redobs))
	!		        if(abs(zobs-zobss(i))>0.001.or.abs(rat-rats(i))>0.0001) then
	!		        	print *, i, zobs, zobss(i), rat, rats(i)
	!		        	stop
	!		        endif
				zobss(i)=zobs; rats(i)=rat
			enddo
			reds(1:numhalo) = zobss(1:numhalo)
			write(tmpstr1, *) int(vlosadds(ivlos)+0.5)
		        nowoutputfile = trim(adjustl(nowinputfile))//'.RSD-radial-selected.vlos'//trim(adjustl(tmpstr1))
                endif
                call random_seed(put = (/0,0/))

                massmin = minval(masss(1:np))
                massmax = maxval(masss(1:np))
                write(*,'(A,4e15.7)') '     Min/Max mass/red = ', real(massmin), real(massmax), &
                        real(minval(reds(1:np))), real(maxval(reds(1:np)))

                masscutfile = trim(adjustl(nowoutputfile))//'.masscut'
                open(unit=3,file=masscutfile,action='write')
                write(3,'(A)') '# Binned mass_cut #fmt: red1, red2, masscut, masscutB(ignore this), tot-#-in-bin, selected-#, goal-#-from-data, #-inbin/#-goal, #-selected/#-goal, #-inbin/#-selected(used as w_fkp)'
                markibin(1:np) = 0
                nowtotleftnum = 0
                do i_bin = 1, numbins
                        red1 = redmins(i_bin)
                        red2 = redmaxs(i_bin)
                        numinbin = 0
                        do i = 1, np
                                if(reds(i)<red2.and.reds(i)>red1) then
                                        numinbin = numinbin + 1
                                        massinbin(numinbin) = masss(i)
                                 endif
                        enddo
                        numgoal = binnednum_target(i_bin)
                        if(numinbin < numgoal) then
                                mass0 = 0.0; massA = 0.0; massB = 0.0; goto 400
                        endif
                        mass1 = minval(massinbin(1:numinbin)); mass2 = maxval(massinbin(1:numinbin)); mass0 = sqrt(mass1*mass2)
!                        write(*,'(i3,A,f8.4,f8.4,i7,i7)'), i_bin, 'th bin: red1,red2 / selected-#-in-bin/ #-goal (data) = ', &
!                                        red1, red2, numinbin, numgoal
                                        
                        i_same = 0; prevnum = -1;
                        do while(.true.)
                                nownum = 0
                                do i = 1, numinbin
                                        if(massinbin(i).ge.mass0) nownum=nownum+1
                                enddo
                                if(nownum .eq. prevnum) then
                                        i_same = i_same +1
                                else
                                        i_same = 0
                                endif
                                prevnum = nownum
                                !write(*,'(A,e14.7,i7,i7,i3)') 'mass-cut / nownum / numgoal / i_same = ', mass0, nownum, numgoal, i_same
                                if(i_same > 10) exit
                                if(nownum > numgoal) then
                                        mass1 = mass0; mass0 = sqrt(mass0*mass2)
                                else
                                        mass2 = mass0; mass0 = sqrt(mass0*mass1)
                                endif
                        enddo

                        ! randomly drop some low mass halos to precisely obtain num we
                        ! want
                        if(nownum > numgoal) then
                               massA = mass0-unitmass; massB = mass0;
                        else
                               massA = mass0; massB = mass0+unitmass;
                        endif
                        numA = 0; numB = 0;
                        do i = 1, numinbin
                                if(massinbin(i).ge.massA) numA = numA + 1
                                if(massinbin(i).ge.massB) numB = numB + 1
                        enddo
                        ABkeeprat = dble(numgoal-numB)/dble(numA-numB)
!                        write(*,'(A,(2e14.7,3i7,f7.3))'), 'massA/massB/numA/numB/numgoal/rat = ', massA, massB, &
!                                numA, numB, numgoal, ABkeeprat
                        massA = mass0; massB = mass0; ! abondon using massA,B...
400	        	continue

                        nownum = 0
                        do i = 1, np
                        	if(reds(i)<red2 .and. reds(i)>red1) then
                                        if(masss(i).ge.massB) then
                                                markibin(i) = i_bin; nownum = nownum + 1
                                        elseif(masss(i).ge.massA) then
                                                call random_number(randx); randx = 0.0
                                                if(randx<ABkeeprat) then
                                                        markibin(i) = i_bin; nownum = nownum + 1
                                                endif
                                        endif
                                endif
                        enddo
                        nowtotleftnum = nowtotleftnum + nownum
                        write(3,'(4e15.7,3i9,3f7.3)') red1, red2, massA, massB, numinbin, nownum, numgoal, &
                                      dble(numinbin)/dble(numgoal), dble(nownum)/dble(numgoal), dble(numinbin) / dble(nownum)
                        wfkp(i_bin) = dble(numinbin) / dble(nownum) ! "WFKP" weight of the galaxy 
!                        write(*,'(A,(4e15.7,i7,i7))') 'red1, red2, massA, massB, nownum, numgoal = ', &
!                                       red1, red2, massA, massB, nownum, numgoal
                enddo
                close(3)
                write(*,'(A,3e14.7)') '     total left-#/goal-#/ratio: ',  real(nowtotleftnum), totgoalnum,  &
                        real(nowtotleftnum/totgoalnum)
                radecoutputfile = trim(adjustl(nowoutputfile))//'.radec'
                open(unit=4, file = nowoutputfile)
                open(unit=5, file = radecoutputfile)
!                write(4, '(A)') '# HR3 mock '//trim(adjustl(tmpstr1))//'. Cosmology: Omegam=0.26,w=-1,h=0.72. Unit: Mpc/h,km/s. Has the same n(z) with DR11v1 # x,y,z,vx,vy,vz,mass,red,ra,dec'
                j = 0
                do i = 1, np
                        if(markibin(i) .eq. 0) cycle
                        call xyz_to_radecr(xs(i),ys(i),zs(i),ra,dec,radius)
			if(extra_radrop) then
				if(ra > extra_radrop_min .and. ra < extra_radrop_max) cycle
			endif
				
                        if(iRSD .eq. 1) then
                                write(4,'(8e15.7,i10)') xs(i), ys(i), zs(i), vxs(i), vys(i), vzs(i), wfkp(markibin(i)), masss(i),  i
                        else
                                write(4,'(8e15.7,i10)') xs(i)*rats(i), ys(i)*rats(i), zs(i)*rats(i), vxs(i), vys(i), vzs(i), &
                                	wfkp(markibin(i) ), masss(i), i
                        endif
                        write(5,*) ra, dec
                        j = j+1
                enddo
                close(4); close(5)
                write(*,'(i11,A)') j, '-lines wrote to: '
                write(*,'(A,A)') '        ', trim(adjustl(nowoutputfile))
                write(*,'(A,A)') '        ', trim(adjustl(radecoutputfile))
        enddo ! do loop of ivlos
        enddo ! do loop of iRSD
    enddo ! do loop of nowpatch
    enddo ! do loop of i_mock
    stop
end program ap_main
