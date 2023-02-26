! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 13:24:30 26/02/2023 |
! +-------------------------------------------+

module io 
    ! 
    ! Input/Output module
    !
    implicit none
    !
    real(kind=8) :: lil, uil, threshold, convR, IRC
    character(len=80) :: printres 
    integer :: uf, dummyvar, findex, sindex, totNGauss, initNGauss
    integer(kind=8) :: totiter
    real(kind=8), dimension(10,10) :: Rmat
    real(kind=8), dimension(:), allocatable :: WGauss, tGauss 

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
        ! Read number of total Gaussian points
        !
        read(newuf,*)
        read(newuf,*) totNGauss
        !
        ! Read initial quadrature point
        !
        read(newuf,*)
        read(newuf,*) initNGauss
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
