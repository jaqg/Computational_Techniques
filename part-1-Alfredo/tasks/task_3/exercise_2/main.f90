! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 12:05:07 11-10-2022 |
! +--------------------------------------------+

program exercise2
    !
    ! 2. Code a program that reads 10 numbers from a file and prints those
    ! bigger than 5.0 and writes -5.0 instead of the numbers lower or equal
    ! than 5.0.
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i
    real(kind=8), dimension(10) :: numbers
    !
    ! Files
    !
    open(unit=10, file="in-exercise2.dat")
    !
    ! Formats
    !
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-------------------+'
    write(*,*) '| Program exercise2 |'
    write(*,*) '+-------------------+'
    write(*,*)
    !
    ! Read input
    !
    read(10,*)
    do i = 1, 10
        read(10,*) numbers(i)
    end do
    !
    ! Write the original numbers
    !
    write(*,*) 'The original numbers are:'
    do i = 1, 10
        write(*,'(f10.2)') numbers(i)
    end do
    write(*,*)
    !
    ! Write the conditioned numbers
    !
    write(*,*) 'The conditioned numbers are:'
    do i = 1, 10
        if (numbers(i)<=5.0) then
            write(*,'(f10.2)') -5.0
        else
            write(*,'(f10.2)') numbers(i)
        end if
    end do
    write(*,*)
    !
    stop
endprogram exercise2
