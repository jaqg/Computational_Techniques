! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 23:04:58 25/02/2023 |
! +---------------------------------------------+

module io 
    ! 
    ! Input/Output module
    !
    implicit none
    !
    real(kind=8) :: lil, uil, threshold, IRC
    character(len=80) :: printres 
    integer :: uf, dummyvar, initN
    integer(kind=8) :: totiter

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
        ! Read value of initial number of subintervals
        !
        read(newuf,*)
        read(newuf,*) initN
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
        write(*,'(A,i0)') 'Initial number of subintervals: N = ', initN
        write(*,*)
        !
        return
    end subroutine print_input 
end module io 
