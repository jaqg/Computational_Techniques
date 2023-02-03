! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 11:15:03 15-10-2022 |
! +---------------------------------------------+

program exercise5
    !
    ! 5. Write (in two different files) a main program and a subroutine so
    ! that the main calls the subroutine, which receives a matrix and returns
    ! it with its diagonal scaled by a factor given in the call to the
    ! subroutine.
    !
    ! Modules
    !
    use mat_mod
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j, ierr, n, factor
    real(kind=8), dimension(:,:), allocatable :: A
    !
    ! Formats
    !
    10 format(' +------ INPUT ------+')
    20 format(' +------ OUTPUT -----+')
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-------------------+'
    write(*,*) '| Program exercise5 |'
    write(*,*) '+-------------------+'
    write(*,*)
    !
    ! Input
    !
    write(*,10)
    write(*,*)
    !
    ! Construction of input matrix
    !
    n = 5 ! Dimension of the matrix
    !
    allocate(A(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of A'
    !
    do i = 1, n
        do j = 1, n
            A(i,j) = dble(i + j)
        end do
    end do
    !
    ! Print input matrix
    !
    write(*,*) 'Input matrix A:'
    call write_mat(A, 6, "F10.2", n)
    write(*,*)
    !
    ! Scale diagonal elemts of matrix A
    !
    factor = 2
    call scale_diag_mat(A, factor, A)
    !
    ! Print output matrix
    !
    write(*,20)
    write(*,*)
    !
    write(*,'(1x,a,1x,i0,a)') 'Diagonal elements scaled by', factor, '.'
    write(*,*)
    !
    write(*,*) 'Output matrix A:'
    call write_mat(A, 6, "F10.2", n)
    write(*,*)
    !
    stop
endprogram exercise5
