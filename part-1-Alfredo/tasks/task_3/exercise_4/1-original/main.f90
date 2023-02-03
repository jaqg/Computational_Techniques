! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Friday 11:49:52 14-10-2022 |
! +-------------------------------------------+

program exercise4
    !
    ! 4. Generate an unformatted file named “input.dat” containing a double
    ! precision vector vec(19) in the first record and a row of a double
    ! precision matrix mat(nrow, 19) in each of the following records.
    ! Write a program (using functions and/or subroutines) that
    ! a) Reads the vector and the matrix. Note that the value of nrow must
    !    be found out to read the full matrix.
    ! b) Calculates the product mat * vec
    ! c) Writes in a “clear form” both the input elements and the final result
    !
    ! Modules
    !
    use mat_mod
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j, ierr, irec, nrow
    character(len=80) :: charvar
    real(kind=8), dimension(19) :: vec
    real(kind=8), dimension(:,:), allocatable :: mat
    real(kind=8), dimension(:,:), allocatable :: C
    real(kind=8) :: xtmp
    !
    ! Formats
    !
    10 format(' +------ INPUT ------+')
    20 format(' +------ OUTPUT -----+')
    !
    ! Files
    !
    open(unit=10, file="input.dat")
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-------------------+'
    write(*,*) '| Program exercise4 |'
    write(*,*) '+-------------------+'
    write(*,*)
    !
    write(*,10)
    write(*,*)
    !
    ! Read input vector
    !
    read(10,*) vec
    !
    ! Write input vector
    !
    write(*,*) 'Vector vec(19):'
    call write_vec(vec, 6, "F10.2")
    write(*,*)
    !
    ! Calculate irec to determine nrow
    !
    irec = 1
    do
        read(10,'(*(a))',end=100) charvar
        irec = irec + 1
    end do
    100 continue
    !
    ! Rewind input
    !
    rewind(10)
    !
    ! Skip first row (vector)
    !
    read(10,*)
    !
    ! Calculate nrow
    !
    nrow = irec - 1
    !
    ! Allocate matrix
    !
    allocate(mat(nrow,19), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of mat'
    !
    ! Read input matrix
    !
    call read_mat(mat, 10, "*")
    !
    ! Write input matrix
    !
    write(*,'(1x,a,i0,a)') 'Matrix mat(', nrow, ',19):'
    call write_mat(mat, 6, "F10.2", 5)
    write(*,*)
    !
    ! Output
    !
    write(*,20)
    write(*,*)
    !
    ! Do product operation: C = mat * vec
    !
    call mat_vec_prod(mat,vec,C)
    !
    ! Write output column matrix C
    !
    write(*,'(1x,a,i0,a)') 'Column matrix C(', nrow, ',1):'
    call write_mat(C, 6, "F15.2", 1)
    write(*,*)
    !
    stop
endprogram exercise4
