module mymodule

    implicit none
    !
    ! 
    !
    contains

    subroutine read_matrix(A, ncol, skiprows, uf, input_format)
        !
        ! A                 real, double precision, array
        ! ncol              integer, single precision
        ! skiprows           integer, single precision
        ! uf                integer, single precision
        ! input_format      string
        !
        ! A: matrix to be printed
        ! ncol: number of columns of the array
        ! skiprows: number of rows to skip BEFORE reading the array
        ! uf: file unit from which the data is readen
        ! input_format: format of the output
        !
        implicit none
        !
        integer, intent(in) :: uf, skiprows, ncol
        character(len=*), intent(in) :: input_format
        character(len=:), allocatable :: actual_format
        real(kind=8), dimension(:,:), allocatable, intent(out) :: A
        !
        ! Dummy variables
        !
        integer :: i, j, ierr, irec, nrow
        character(len=80) :: charvar 
        !
        ! Calculate irec to determine number of rows of 'A' (nrows)
        !
        irec = 0
        do
            read(uf,'(a)',end=100) charvar
            irec = irec + 1
        end do
        100 continue
        !
        ! Calculate number of rows, nrow
        !
        nrow = irec - skiprows
        !
        ! Rewind input
        !
        rewind(uf)
        !
        ! Allocate matrix
        !
        allocate(A(nrow,ncol), stat=ierr)
        if (ierr .ne. 0) stop 'read_matrix: Error in allocation of A'
        !
        ! Skip rows
        !
        srl1: do i = 1, skiprows
            read(uf,*) 
        end do srl1 
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

    subroutine write_mat(A, file_unit, output_format, cpr)
        !
        ! A                 real, double precision, array
        ! file_unit         integer, single precision
        ! input_format      string
        ! cpr               integer, single precision
        !
        ! A: matrix to be printed
        ! file_unit: file unit to print the matrix
        ! output_format: format of the output
        ! cpr: number of columns printed per row
        !
        implicit none
        !
        real(kind=8), dimension(:,:), intent(in) :: A
        integer, intent(in) :: file_unit
        character(len=*), intent(in) :: output_format
        character(len=:), allocatable :: actual_format
        integer :: i, j, k, nrow, ncol, cpr, iter, mincol, maxcol
        !
        nrow = size(A,1)
        ncol = size(A,2)
        !
        iter = ncol/cpr
        !
        if (output_format == "*") then
            if (cpr == ncol) then
                do i = 1, nrow
                    write(file_unit,*) ( A(i,j), j=1, ncol )
                end do
            else
                do k=1, iter
                    !
                    mincol = (k - 1) * cpr + 1
                    maxcol = k * cpr
                    !
                    write(file_unit,'(1x,a,1x,i0,a,i0)') 'Columns', &
                    mincol, '-', maxcol
                    !
                    do i = 1, nrow
                        write(file_unit,*) (A(i,j), j=mincol,maxcol)
                    end do
                    !
                end do
                !
                mincol = iter * cpr + 1
                maxcol = ncol
                !
                write(file_unit,'(1x,a,1x,i0,a,i0)') 'Columns', &
                mincol, '-', maxcol
                !
                do i = 1, nrow
                    write(file_unit,*) (A(i,j),j=mincol,maxcol)
                end do
            end if
        else
            !
            actual_format = "(*(" // output_format // "))"
            !
            if (cpr == ncol) then
                do i = 1, nrow
                    write(file_unit,actual_format) ( A(i,j), j=1, ncol )
                end do
            else
                do k=1, iter
                    !
                    mincol = (k - 1) * cpr + 1
                    maxcol = k * cpr
                    !
                    write(file_unit,'(1x,a,1x,i0,a,i0)') 'Columns', &
                    mincol, '-', maxcol
                    !
                    do i = 1, nrow
                        write(file_unit,actual_format) (A(i,j),j=mincol,maxcol)
                    end do
                    !
                end do
                !
                mincol = iter * cpr + 1
                maxcol = ncol
                !
                write(file_unit,'(1x,a,1x,i0,a,i0)') 'Columns', &
                mincol, '-', maxcol
                !
                do i = 1, nrow
                    write(file_unit,actual_format) (A(i,j),j=mincol,maxcol)
                end do
            end if
            !
        end if
        !
        return
    end subroutine write_mat
end module mymodule
