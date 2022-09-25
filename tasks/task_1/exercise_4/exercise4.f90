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
    !
    ! Formats
    !
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-------------------+'
    write(*,*) '| Program exercise4 |'
    write(*,*) '+-------------------+'
    write(*,*)
    !
    ! Read input
    !
    read(10,*)
    read(10,*) n
    allocate(numbers(n))
    read(10,*)
    read(10,*) numbers
    !
    ! Print input set of numbers
    !
    write(*,*) 'The input set of numbers is:'
    write(*,'(f10.2)') numbers(:)
    write(*,*)
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
    write(*,*) 'The sorted set of numbers in ascending order is:'
    write(*,'(f10.2)') numbers(:)
    write(*,*)
    !
    stop
endprogram exercise4
