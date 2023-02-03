module mymodule

    implicit none
    !
    contains

    subroutine read_sq_matrix(M, skiprows, unf, input_format)
        !
        ! Subroutine to read a SQUARE matrix from input file
        !
        ! M                 real, double precision, array
        ! skiprows          integer, single precision
        ! unf               integer, single precision
        ! input_format      tring
        !
        ! M: matrix to be printed
        ! skiprows: number of rows to skip BEFORE reading the array
        ! unf: file unit from which the data is readen
        ! input_format: format of the output
        !
        implicit none
        !
        integer, intent(in) :: unf, skiprows
        character(len=*), intent(in) :: input_format
        character(len=:), allocatable :: actual_format
        real(kind=8), dimension(:,:), allocatable, intent(out) :: M
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
            read(unf,'(a)',end=100) charvar
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
        rewind(unf)
        !
        ! Allocate matrix
        !
        allocate(M(nrow,nrow), stat=ierr)
        if (ierr .ne. 0) stop 'read_matrix: Error in allocation of M'
        !
        ! Skip rows
        !
        srl1: do i = 1, skiprows
            read(unf,*) 
        end do srl1 
        !
        ! Read matrix
        !
        if (input_format == "*") then
            do i = 1, nrow
                ! read(unf,*) ( M(i,j), j=1, nrow )
                read(unf,*) M(i,:)
            end do
        else
            actual_format = "(*(" // input_format // "))"
            do i = 1, nrow
                read(unf,actual_format) ( M(i,j), j=1, nrow )
            end do
        end if
        !
        return
    end subroutine read_sq_matrix 
    !
    subroutine read_vec(v, skiprows, unf, input_format)
        !
        ! Subroutine to read a vector from input file
        !
        ! v                 real, double precision, array
        ! skiprows          integer, single precision
        ! unf               integer, single precision
        ! input_format      tring
        !
        ! v: vector to be printed
        ! skiprows: number of rows to skip BEFORE reading the array
        ! unf: file unit from which the data is readen
        ! input_format: format of the output
        !
        implicit none
        !
        integer, intent(in) :: unf, skiprows
        character(len=*), intent(in) :: input_format
        character(len=:), allocatable :: actual_format
        real(kind=8), dimension(:), allocatable, intent(out) :: v
        !
        ! Dummy variables
        !
        integer :: i, ierr, irec, n
        character(len=80) :: charvar 
        !
        ! Calculate irec to determine number of rows of 'A' (nrows)
        !
        irec = 0
        do
            read(unf,'(a)',end=100) charvar
            irec = irec + 1
        end do
        100 continue
        !
        ! Calculate number of elements, n
        !
        n = irec - skiprows
        !
        ! Rewind input
        !
        rewind(unf)
        !
        ! Allocate matrix
        !
        allocate(v(n), stat=ierr)
        if (ierr .ne. 0) stop 'read_vec: Error in allocation of M'
        !
        ! Skip rows
        !
        srl1: do i = 1, skiprows
            read(unf,*) 
        end do srl1 
        !
        ! Read matrix
        !
        if (input_format == "*") then
            do i = 1, n
                read(unf,*) v(i)
            end do
        else
            actual_format = "(*(" // input_format // "))"
            do i = 1, n
                read(unf,actual_format) v(i)
            end do
        end if
        !
        return
    end subroutine read_vec 
    !
    subroutine write_mat(M, file_unit, output_format, cpr)
        !
        ! M                 real, double precision, array
        ! file_unit         integer, single precision
        ! input_format      string
        ! cpr               integer, single precision
        !
        ! M: matrix to be printed
        ! file_unit: file unit to print the matrix
        ! output_format: format of the output
        ! cpr: number of columns printed per row
        !
        implicit none
        !
        real(kind=8), dimension(:,:), intent(in) :: M
        integer, intent(in) :: file_unit
        character(len=*), intent(in) :: output_format
        character(len=:), allocatable :: actual_format
        integer :: i, j, k, nrow, ncol, cpr, iter, mincol, maxcol
        !
        nrow = size(M,1)
        ncol = size(M,2)
        !
        iter = ncol/cpr
        !
        if (output_format == "*") then
            if (cpr == ncol) then
                do i = 1, nrow
                    write(file_unit,*) ( M(i,j), j=1, ncol )
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
                        write(file_unit,*) (M(i,j), j=mincol,maxcol)
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
                    write(file_unit,*) (M(i,j),j=mincol,maxcol)
                end do
            end if
        else
            !
            actual_format = "(*(" // output_format // "))"
            !
            if (cpr == ncol) then
                do i = 1, nrow
                    write(file_unit,actual_format) ( M(i,j), j=1, ncol )
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
                        write(file_unit,actual_format) (M(i,j),j=mincol,maxcol)
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
                    write(file_unit,actual_format) (M(i,j),j=mincol,maxcol)
                end do
            end if
            !
        end if
        !
        return
    end subroutine write_mat
    !
    subroutine write_vec(v, file_unit, output_format)
        !
        ! Subroutine to print a vector
        !
        ! v                 real, double precision, array
        ! file_unit         integer, single precision
        ! input_format      string
        !
        ! v: matrix to be printed
        ! file_unit: file unit to print the matrix
        ! output_format: format of the output
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: v
        integer, intent(in) :: file_unit
        character(len=*), intent(in) :: output_format
        character(len=:), allocatable :: actual_format
        integer :: n
        !
        n = size(v,1)
        !
        if (output_format == "*") then
            !
            write(file_unit,*) v
            !
        else
            !
            actual_format = "(*(" // output_format // "))"
            !
            write(file_unit,actual_format) v
            !
        end if
        !
        return
    end subroutine write_vec
 
end module mymodule
