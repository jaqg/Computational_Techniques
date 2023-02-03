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
    use random_numbers
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j, ierr, randnrows, irec, nrow, ncols
    character(len=80) :: charvar
    integer, parameter :: prec=8
    real(kind=prec), dimension(:), allocatable :: invec
    real(kind=prec), dimension(:,:), allocatable :: inmat
    real(kind=prec), dimension(:), allocatable :: vec
    real(kind=prec), dimension(:,:), allocatable :: mat
    real(kind=prec), dimension(:,:), allocatable :: C
    !
    ! Formats
    !
    10 format(' +------ INPUT ------+')
    20 format(' +------ OUTPUT -----+')
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
    !
    ! PROCEDURE:
    !
    ! I am going to create an input vector, invec, and an input matrix, inmat
    ! of dimensions (where randnrows is a random integer between 1-10)
    ! invec -> invec(19)    inmat -> inmat(randnrows, 19)
    ! and give values to them. Then, I am going to write them to the file
    ! "input.dat" and proceed to read and store the first row in 'vec', and
    ! the rest of the file in 'mat', with which I am going to do the calc.
    ! Also, we can check if the number of rows assigned to mat after reading
    ! is equal to randnrows, as so the reading procedure is working well.
    !
    !
    ! Define number of rows of the matrix randomly between 1-10
    ! and number of columns to 19
    !
    randnrows = random_integer(1,10)
    ncols = 19
    !
    write(*,'(1x,a,i0)') &
     & 'The number of rows of the matrix, generated randomly, is: ', randnrows
    write(*,*)
    !
    ! Allocate input arrays
    !
    allocate(invec(ncols), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of invec'
    !
    allocate(inmat(randnrows,ncols), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of inmat'
    !
    ! Generate input.dat file
    !
    !
    ! Open/create "input.dat" as unformatted
    !
    open(unit=10, file="input.dat", form='unformatted', &
                & action='write', access='sequential')
    !
    ! Create 'invec' and 'inmat' as, for example:
    !
    l1: do i = 1, randnrows
        l2: do j = 1, 19
            invec(j) = dble(j)
            inmat(i,j) = dble(i+j)
        end do l2
    end do l1
    !
    ! Write 'invec' in the first record of the input file, and 'inmat' as
    ! the rest of the records
    !
    write(10) invec
    do i = 1, randnrows
        write(10) inmat(i,:)
    end do
    !
    ! Once the file is written, close it but keep it
    !
    close(10, status='keep')
    !
    ! Re-open the file to read 'vec' and 'A' from it
    !
    open(unit=10, file="input.dat", status='old', form='unformatted', &
                & action='read', access='sequential')
    !
    ! Read 'vec'
    !
    allocate(vec(ncols), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of vec'
    !
    read(10) vec
    !
    ! Write input vector
    !
    write(*,*) 'Vector vec(19):'
    call write_vec(vec, 6, "F10.2")
    write(*,*)
    !
    ! Calculate irec to determine number of rows of 'A' (nrows)
    !
    irec = 1
    do
        read(10,end=100) charvar
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
    read(10)
    !
    ! Calculate nrow
    !
    nrow = irec - 1
    write(*,'(a,i0)') 'The number of rows of the matrix, calculated inside the &
              & program is: ', nrow
    write(*,*)
    !
    ! Allocate matrix
    !
    allocate(mat(nrow,19), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of mat'
    !
    ! Read input matrix
    !
    call read_mat(mat, 10, "unformatted")
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
