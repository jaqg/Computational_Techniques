! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Friday 17:06:14 09-12-2022 |
! +-------------------------------------------+

module io
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j, N, uf, dummyvar
    character(len=89) :: printres
    real(kind=8) :: lil, uil, h, threshold
    real(kind=8) :: IR, IRC, prevIRC, diffIRC, xi, xtmp
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
            ! Read value of lower integration limit
            !
            read(uf,*)
            read(uf,*) lil
            !
            ! Read value of upper integration limit
            !
            read(uf,*)
            read(uf,*) uil
            !
            ! Read value of threshold of convergence
            !
            read(uf,*)
            read(uf,*) threshold
            !
            ! Close the file
            !
            close(uf)
            !
            return
        end subroutine read_input
end module
