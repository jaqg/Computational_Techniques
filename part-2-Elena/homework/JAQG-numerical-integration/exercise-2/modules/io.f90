! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Saturday 11:46:01 03-12-2022 |
! +-------------------------------------------+

module io
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j, uf
    character(len=80) :: printres
    integer(kind=8) :: Ncircle, Ntotal
    real(kind=8) :: x, y
    real(kind=8) :: h, d, drecvex
    real(kind=8) :: pi, pical, errpi
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
            ! Read value of h
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
end module
