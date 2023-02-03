! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 18:30:32 05-10-2022 |
! +----------------------------------------------+

program matrix_multiplication
    !
    ! Write a Fortran program to calculate the product of the two matrices
    ! given as input. The input should also include the dimensions of the
    ! input matrices, so that the first step in the program will be to check
    ! whether such matrices can actually be multiplied.
    !
    ! The elements of the product nxn matrix C = AB are given by
    ! C_i,j = sum_k=1^n A_i,k B_k,j
    !
    ! Modules
    !
    use mat_mod
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j, k, ierr
    logical :: T, F
    integer :: nrowA, ncolA, nrowB, ncolB, n
    real(kind=8), dimension(:,:), allocatable :: A, B, C
    !
    ! Files
    !
    open(unit=10, file="in-matrix.dat")
    open(unit=11, file="out-main.dat")
    !
    ! === START OF THE PROGRAM ===
    !
    write(11,*) '+-------------------------------+'
    write(11,*) '| Program matrix_multiplication |'
    write(11,*) '+-------------------------------+'
    write(11,*)
    !
    ! Read input and allocate matrices
    !
    read(10,*)
    read(10,*) nrowA, ncolA
    read(10,*)
    !
    allocate(A(nrowA,ncolA), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of A'
    !
    call read_mat(A, 10, "*")
    !
    read(10,*)
    read(10,*) nrowB, ncolB
    read(10,*)
    !
    allocate(B(nrowB,ncolB), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of B'
    !
    call read_mat(B, 10, "*")
    !
    ! Close input files
    !
    close(10)
    !
    ! Print input matrices
    !
    write(11,*) 'The input matrices are:'
    write(11,*)
    write(11,*) 'A ='
    call write_mat(A, 11, "f6.1")
    write(11,*)
    write(11,*) 'B ='
    call write_mat(B, 11, "f6.1")
    write(11,*)
    !
    ! Product C = AB calculation
    !
    call matrix_mult(A, B, C)
    !
    ! Print result matrix C
    !
    write(11,*) 'The result matrix C = AB is:'
    write(11,*)
    write(11,*) 'C ='
    call write_mat(C, 11, "f10.2")
    write(11,*)
    !
    ! Close output file
    !
    close(11)
    !
    write(*,*) 'Results writen to file "out-main.dat"'
    !
    stop
endprogram matrix_multiplication
