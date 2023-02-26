! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 00:37:08 26/02/2023 |
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

    subroutine print_input
        !
        ! Subroutine to print the input parameters
        !
        implicit none
        !
        write(*,'(A,f10.4)') 'Lower integration limit: ', lil
        write(*,'(A,f10.4)') 'Upper integration limit: ', uil
        write(*,*)
        write(*,'(A,e12.2)') 'Threshold for convergence:', threshold
        write(*,*)
        !
        return
    end subroutine print_input 
end module io 
