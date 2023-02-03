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
    real(kind=8) :: x0, threshold, h, xmin, first_diff
    integer :: totiter
    character(len=80) :: fin_diff_method
    !
    ! Subprograms
    !
    contains
        subroutine read_input
            !
            ! Subroutine to read input
            !
            implicit none
            !
            integer :: uf
            !
            open(newunit=uf,file="data/input.dat",action="read",status='old')
            !
            ! Read value of initial point x_0
            !
            read(uf,*)
            read(uf,*) x0
            !
            ! Read value of threshold for convergence
            !
            read(uf,*)
            read(uf,*) threshold
            !
            ! Read method for the finite difference
            !
            read(uf,*)
            read(uf,*) fin_diff_method
            fin_diff_method = trim(fin_diff_method)
            !
            ! Read value of spacing 'h'
            !
            read(uf,*)
            read(uf,*) h
            !
            ! Close the file
            !
            close(uf)
            !
            return
        end subroutine read_input
        !
end module
