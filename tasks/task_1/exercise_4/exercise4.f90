! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 22:04:08 25-09-2022 |
! +-------------------------------------------+

program exercise4
    !
    ! Write a Fortran program to sort a set of numbers in ascending order.
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j
    integer :: n
    real(kind=8), dimension(:), allocatable :: numbers
    real(kind=8) :: a
    !
    ! Files
    !
    open(unit=10, file="in-exercise4.dat")
    open(unit=11, file="out-exercise4.dat")
    !
    ! Formats
    !
    !
    ! === START OF THE PROGRAM ===
    !
    write(11,*) '+-------------------+'
    write(11,*) '| Program exercise4 |'
    write(11,*) '+-------------------+'
    write(11,*)
    !
    ! Read input
    !
    read(10,*)
    read(10,*) n
    allocate(numbers(n))
    read(10,*)
    read(10,*) numbers
    !
    ! Close input file
    !
    close(10)
    !
    ! Print input set of numbers
    !
    write(11,*) 'The input set of numbers is:'
    write(11,'(f10.2)') numbers(:)
    write(11,*)
    !
    ! Sorted array in ascending order
    !
    loop1: do i = 0, n
        loop2: do j = i+1, n
            if (numbers(i) > numbers(j)) then
                a = numbers(i)
                numbers(i) = numbers(j)
                numbers(j) = a
            end if
        end do loop2
    end do loop1
    !
    ! Print results
    !
    write(11,*) 'The sorted set of numbers in ascending order is:'
    write(11,'(f10.2)') numbers(:)
    !
    ! Close output file
    !
    close(11)
    !
    write(*,*)
    write(*,*) 'Results writen to file "out-exercise4.dat"'
    write(*,*)
    !
    stop
endprogram exercise4
