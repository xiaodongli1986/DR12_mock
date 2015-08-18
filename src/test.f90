!####################################
! scan chisqs
!####################################

! Modify this file name to main.f90, complie and run, you get the rotaion matrix and vector after rotation:
!
!  0.6520163      0.4695025      0.5953505    
!  0.5556061     -0.8301617      4.6189241E-02
!  0.5159231      0.3006642     -0.8021374    
!  -36.91167       17.84553      -70.29727 

program ap_main

!use mpi
use ap_cosmo_funs

	implicit none

        integer :: i
        real(dl) :: thetax, thetay, thetaz, Point(3), NewPoint(3), Matrix(3,3)


	thetax = -34.5
	thetay = -44.62
	thetaz = -32.04

        call RotateMatrix(thetax,thetay,thetaz,Matrix)
        do i = 1, 3
                print *, real(Matrix(i,1)),  real(Matrix(i,2)), &
                         real(Matrix(i,3))
        enddo
        
        Point = (/-57.54_dl,-38.57_dl, 42.71_dl /)
        call RotateM(Point, Matrix, NewPoint)
        print *, real(NewPoint(1:3))
end program ap_main
