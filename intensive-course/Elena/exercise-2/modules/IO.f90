! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Monday 15:17:07 16/01/2023 |
! +-------------------------------------------+

module io 
    ! 
    ! Input/Output module
    !
    implicit none
    !
    real(kind=8) :: lil, uil, threshold, convR
    character(len=80) :: printres 
    integer :: uf, dummyvar, findex, sindex
    integer(kind=8) :: totiter
    real(kind=8), dimension(10,10) :: Rmat

    contains

    subroutine read_input
        !
        ! Subroutine to read input
        !
        implicit none
        !
        integer :: newuf
        !
        open(newunit=newuf,file="data/input.dat",action="read",status='old')
        !
        ! Read value of lower integration limit
        !
        read(newuf,*)
        read(newuf,*) lil
        !
        ! Read value of upper integration limit
        !
        read(newuf,*)
        read(newuf,*) uil
        !
        ! Read value of threshold of convergence
        !
        read(newuf,*)
        read(newuf,*) threshold
        !
        ! Close the file
        !
        close(newuf)
        !
        return
    end subroutine read_input
end module io 
