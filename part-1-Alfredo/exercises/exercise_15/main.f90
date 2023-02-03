! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Thursday 11:36:01 06-10-2022 |
! +---------------------------------------------+

program exercise15
    !
    ! 15. Use to previous procedures to write a program that Cholesky
    ! decompose the matrix ∑_i=1^3 𝑣_i 𝑤_i , where 𝑣_i are column
    ! vectors built with elements calculated using Fibonacci series
    ! and 𝑤_i are row vectors built with elements calculated by
    ! summing successions of powers of inverse natural numbers.
    ! The output must look nice.
    !
    ! Modules
    !
    use cholesky_mod
    use mat_mod
    use successions_mod
    use fibonacci_series
    use random_numbers
    !
    ! Variable definition
    !
    implicit none
    integer :: i, j, n, ierr
    real(kind=8), allocatable, dimension(:,:) :: v, w, A, A_aux, A_sym, L
    !
    ! Files
    !
    !
    ! Formats
    !
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+--------------------+'
    write(*,*) '| Program exercise15 |'
    write(*,*) '+--------------------+'
    write(*,*)
    !
    ! Input
    !
    ! write(*,*) 'Dimensions of the matrix nxn'
    ! write(*,*) 'n?' ; read(*,*) n
    ! write(*,*)
    n = 3
    !
    allocate(v(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of v'
    !
    allocate(w(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of w'
    !
    ! Construct v
    !
    loopv1: do i = 1, n
        loopv2: do j = 1, n
            v(i,j) = fibo_n(random_integer(1,5))
        end do loopv2
    end do loopv1
    !
    ! Write v
    !
    write(*,*) 'Matrix v:'
    call write_mat(v, 6, "F10.2")
    write(*,*)
    !
    ! Construct w
    !
    loopw1: do i = 1, n
        loopw2: do j = 1, n
            call ssopoinn(random_integer(1,5), random_integer(1,5), w(j,i))
        end do loopw2
    end do loopw1
    !
    ! Write w
    !
    write(*,*) 'Matrix w:'
    call write_mat(w, 6, "F10.4")
    write(*,*)
    !
    ! Construct A
    !
    call mat_outer_prod(v, w, A)
    !
    ! Write A
    !
    write(*,*) 'Matrix A:'
    call write_mat(A, 6, "F10.4")
    write(*,*)
    !
    ! Symmetrize A
    !
    allocate(A_sym(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of A_sym'
    !
    call sym_mat(A, A_sym)
    !
    ! Write A symmetrized
    !
    write(*,*) 'Matrix A (symmetrized):'
    call write_mat(A_sym, 6, "F10.4")
    write(*,*)
    !
    ! Cholesky-decompose A
    !
    allocate(L(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of L'
    !
    write(*,*) 'Cholesky decomposition of A:'
    call cholesky(A_sym, L, 6, "F10.4")
    !
    stop
endprogram exercise15
