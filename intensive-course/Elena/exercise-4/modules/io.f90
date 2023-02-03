! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 00:48:51 18-12-2022 |
! +-------------------------------------------+

module io
    !
    ! Variable definition
    !
    implicit none
    !
    character(len=80) :: fin_diff_method
    integer :: uf, ncoord, maxiter
    real(kind=8) :: x0, y0, h  
    real(kind=8), dimension(:,:), allocatable :: coord, grad
    !
    ! Subprograms
    !
    contains
    !
    subroutine read_input
        !
        ! Subroutine to read input
        !
        implicit none
        !
        integer :: nuf
        !
        open(newunit=nuf,file="data/input.dat",action="read",status='old')
        !
        ! Read number of coordinates
        !
        read(nuf,*)
        read(nuf,*) ncoord
        !
        ! Read value of initial point x_0
        !
        read(nuf,*)
        read(nuf,*) x0
        !
        ! Read value of initial point y_0
        !
        read(nuf,*)
        read(nuf,*) y0
        !
        ! Read value of fixed step size
        !
        read(nuf,*)
        read(nuf,*) h
        !
        ! Read value of maximum value of iterations
        !
        read(nuf,*)
        read(nuf,*) maxiter
        !
        ! Read method for the finite difference
        !
        read(nuf,*)
        read(nuf,*) fin_diff_method
        fin_diff_method = trim(fin_diff_method)
        !
        ! Close the file
        !
        close(nuf)
        !
        return
    end subroutine read_input
    !
end module
