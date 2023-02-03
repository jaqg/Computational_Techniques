! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Monday 11:15:07 17-10-2022 |
! +-------------------------------------------+

program exercise8
    !
    ! 8. Write a Makefile to compile and link the procedures from exercises
    ! 5 and 6.
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
    real(kind=8) :: numcheck
    !
    ! Files
    !
    !
    ! Formats
    !
    10 format(' +------ INPUT ------+')
    20 format(' +------ OUTPUT -----+')
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-------------------+'
    write(*,*) '| Program exercise8 |'
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
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of A'
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
    ! Scale diagonal elements of matrix A by factor
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
    ! Normalisation of A
    !
    A = norm_mat(A)
    !
    write(*,*) 'Normalised matrix A:'
    call write_mat(A, 6, "F10.2", n)
    write(*,*)
    !
    write(*,*) 'Numerical check of normalisation:'
    ! do i = 1, n
    !     numcheck = magn_vec( A(:,i) / magn_vec( A(:,i) ) )
    !     if (numcheck == 1.0_8) then
    !         write(*,'(a,i0,a,i0,a,f6.4,a)') &
    !                       ' | A(:,',i,')/|A(:,',i,')| | = ', numcheck, ' -> OK'
    !     else
    !         write(*,'(a,i0,a,i0,a,f6.4,a)') &
    !                       ' | A(:,',i,')/|A(:,',i,')| | = ', numcheck, ' -> X'
    !     end if
    ! end do
    !
    ! write(*,*) 'Or:'
    !
    do i = 1, n
        write(*,'(1x,a,i0,a,f18.16)') 'sum(|A(:,',i,')|**2) = ', &
                                     & sum(abs(A(:,i))**2)
    end do
    !
    write(*,*)
    !
    stop
endprogram exercise8
