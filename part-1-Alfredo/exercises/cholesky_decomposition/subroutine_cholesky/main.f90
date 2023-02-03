! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 13:24:51 05-10-2022 |
! +----------------------------------------------+

program main
    !
    ! A program to construct a nxn real symetric positive definite matrix and
    ! decompose it with Cholesky method
    !
    !
    ! Modules
    !
    use cholesky_mod
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, J, k, ierr
    integer :: filA, colB
    integer :: n, totiter
    real(kind=8), dimension(:,:), allocatable :: M, L
    !
    ! Files
    !
    open(unit=10, file="in-matrix.dat")
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+------------------+'
    write(*,*) '| Program Cholesky |'
    write(*,*) '+------------------+'
    write(*,*)
    !
    ! Read and print dimension input
    !
    read(10,*) ! Blank line
    read(10,*) n
    write(*,*) 'The dimension of the matrix is'
    write(*,'(1x,a,1x,i4.1)') 'n =', n
    write(*,*)
    !
    ! Allocate matrices
    !
    allocate(M(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of M'
    !
    allocate(L(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of L'
    !
    ! Read and print vector input
    !
    read(10,*)
    do i = 1, n
        read(10,*) ( M(i,j), j=1,n )
    end do
    !
    ! Print the original matrix
    !
    write(*,form10)
    write(*,*) 'Original matrix, M0'
    lom1: do i = 1, n
        write(*,'(1000f6.1)') ( M(i,j), j=1, n )
    end do lom1
    write(*,form10)
    write(*,*)
    !
    ! Main loop
    !
    write(*,*)
    write(*,*) '--- ITERATIONS ---'
    write(*,*)
    !
    call cholesky(n,M,L,totiter)
    !
    write(*,*)
    write(*,*) '--- FINAL RESULTS ---'
    write(*,*)
    !
    write(*,'(1x,a,1x,i5.1)') 'Total number of iterations:', totiter
    write(*,*)
    !
    ! Print matrix L
    !
    write(*,form10)
    write(*,*) 'Matrix L'
    do i=1, n
        write(*,'(1000f6.1)') L(i,:)
    end do
    write(*,form10)
    !
    stop
endprogram main
