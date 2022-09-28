! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 13:03:07 27-09-2022 |
! +--------------------------------------------+

program exercise10
    !
    ! Write a program that can write in a nice style a real matrix of
    ! 30 rows and 30 columns
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j
    integer :: n
    real(kind=8), dimension(:,:), allocatable :: A
    !
    ! Files
    !
    !
    ! Formats
    !
    10 format(' -------------------------------------------------')
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+--------------------+'
    write(*,*) '| Program exercise10 |'
    write(*,*) '+--------------------+'
    write(*,*)
    !
    ! Dimension of the matrix
    !
    n = 30
    !
    ! Create matrix
    !
    allocate(A(n,n))
    loop1: do i = 1, n
        loop2: do j = 1, n
            A(i,j) = dble(i + j) -1.0_8
        end do loop2
    end do loop1
    !
    ! Write matrix, 5 columns at a time
    !
    ! write(*,'(10000f10.5)') A
    do j = 1, n, 5
        write(*,10)
        write(*,'(1x,a,1x,i2,1x,a,1x,i2)') 'Columns', j, 'to', j+4
        ! write(*,'(1x,a,1x,5i8)') "#Col.:", (j+i, i=0,4)
        write(*,10)
        do i = 1, n
            write(*,'(10000f10.4)') A(i,j:j+4)
        end do
        write(*,10)
        write(*,*)
    end do
    !
    stop
endprogram exercise10
