module mymodule

    implicit none
    !
    ! 
    !
    contains

    subroutine read_matrix(uf, A, input_format)
        ! use stdlib_ascii, only: is_blank
        implicit none
        integer, intent(in) :: uf
        character(len=*), intent(in) :: input_format
        character(len=:), allocatable :: actual_format
        real(kind=8), dimension(:,:), allocatable, intent(out) :: A
        !
        ! Dummy variables
        !
        integer :: i, j, ierr, irec, nrow, ncol, ios
        character :: charvar
        logical :: lastblank
        integer, dimension(100) :: dummy_a
        integer, parameter :: signal = -huge(dummy_a)
        !
        ! Calculate irec to determine number of rows of 'A' (nrows)
        !
        irec = 1
        do
            read(uf,'(a)',end=100) charvar
            irec = irec + 1
        end do
        100 continue
        !
        ! Calculate number of row, nrow
        !
        nrow = irec - 1
        !
        ! Rewind input
        !
        rewind(uf)
        !
        ! Calculate number of columns 
        ! (adapted from https://github.com/fortran-lang/stdlib/blob)
        ! the principle is the same as number of rows
        !
        ncol = 0
        dummy_a = signal
        read (unit=*, fmt=*, iostat=ios) dummy_a
        ncol = count(dummy_a/=signal)
        ! print *, dummy_a(:count(dummy_a/=signal))        
        !
        ! Allocate matrix
        !
        allocate(A(nrow,ncol), stat=ierr)
        if (ierr .ne. 0) stop 'read_matrix: Error in allocation of A'
        !
        ! Read matrix
        !
        if (input_format == "*") then
            do i = 1, nrow
                read(uf,*) ( A(i,j), j=1, ncol )
            end do
        else
            actual_format = "(*(" // input_format // "))"
            do i = 1, nrow
                read(uf,actual_format) ( A(i,j), j=1, ncol )
            end do
        end if
        !
        return
    end subroutine read_matrix 


    ! subroutine read_matrix(uf, A, input_format)
    !     implicit none
    !     integer, intent(in) :: uf
    !     character(len=*), intent(in) :: input_format
    !     character(len=:), allocatable :: actual_format
    !     real(kind=8), dimension(:,:), allocatable, intent(out) :: A
    !     !
    !     ! Dummy variables
    !     !
    !     integer :: i, j, ierr, irec, nrow, ncol, ios
    !     character(len=80) :: charvar
    !     logical :: lastblank
    !     !
    !     ! Calculate irec to determine number of rows of 'A' (nrows)
    !     !
    !     irec = 1
    !     do
    !         read(uf,'(a)',end=100) charvar
    !         irec = irec + 1
    !     end do
    !     100 continue
    !     !
    !     ! Calculate number of row, nrow
    !     !
    !     nrow = irec - 1
    !     !
    !     ! Rewind input
    !     !
    !     rewind(uf)
    !     !
    !     ! Calculate number of columns 
    !     ! (adapted from https://github.com/fortran-lang/stdlib/blob)
    !     ! the principle is the same as number of rows
    !     !
    !     ! ncol = 0
    !     ! lastblank = .true.
    !     ! do
    !     !     read(uf, '(a)', advance='no', iostat=ios) charvar
    !     !     if (ios /= 0) exit
    !     !     if (lastblank .and. .not. is_blank(charvar)) ncol = ncol + 1
    !     !     lastblank = is_blank(charvar)
    !     ! end do
    !     irec = 1
    !     do
    !         read(uf, '(a)', advance='no', end=200) charvar
    !         irec = irec + 1
    !     end do
    !     200 continue
    !     !
    !     ! Calculate number of columns, ncol
    !     !
    !     ncol = irec - 1
    !     !
    !     ! Rewind the file
    !     !
    !     rewind(uf)
    !     !
    !     ! Allocate matrix
    !     !
    !     allocate(A(nrow,ncol), stat=ierr)
    !     if (ierr .ne. 0) stop 'read_matrix: Error in allocation of A'
    !     !
    !     ! Read matrix
    !     !
    !     if (input_format == "*") then
    !         do i = 1, nrow
    !             read(uf,*) ( A(i,j), j=1, ncol )
    !         end do
    !     else
    !         actual_format = "(*(" // input_format // "))"
    !         do i = 1, nrow
    !             read(uf,actual_format) ( A(i,j), j=1, ncol )
    !         end do
    !     end if
    !     !
    !     return
    ! end subroutine read_matrix 

end module mymodule
