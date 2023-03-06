! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 00:20:21 01/03/2023 |
! +----------------------------------------------+

module io 
    ! 
    ! Input/Output module
    !
    implicit none
    !
    integer :: uf, poldeg
    character(len=80) :: fmt, data_file_name, elim_method
    real(kind=8) :: tolerance, R2 
    real(kind=8), dimension(:,:), allocatable :: datmat, augmat
    real(kind=8), dimension(:), allocatable :: coefs
    real(kind=8), dimension(3,3) :: prueba, pruebainv
    !
    contains

    subroutine read_input
        !
        ! Subroutine to read the input
        !
        implicit none
        !
        integer :: iuf, datauf, ios
        !
        open(newunit=iuf, file='./data/input.dat', iostat=ios, status="old", &
        & action="read")
        if (ios /= 0) stop "Error opening file 'data/input.dat'"
        !
        ! Read the elimination method (Jordan, backsubstitution)
        !
        read(iuf,*)
        read(unit=iuf, fmt="(a)", iostat=ios) elim_method
        if (ios /= 0) stop "Read error in file unit iuf"
        !
        elim_method = trim( elim_method )
        !
        ! Read the input file with the data points
        !
        read(iuf,*)
        read(unit=iuf, fmt="(a)", iostat=ios) data_file_name
        if (ios /= 0) stop "Read error in file unit iuf"
        !
        data_file_name = trim( data_file_name )
        !
        ! Read data from the data file and store in 'datmat' matrix
        !
        open(newunit=datauf, file=data_file_name, iostat=ios, status="old", &
        & action="read")
        if (ios /= 0) stop "Error opening file '$data_file_name'"
        !
        call read_matrix(datauf, datmat, 2, 1, "*")
        !
        ! Read the degree of the polynomial from the input file
        !
        read(iuf,*)
        read(unit=iuf, fmt='(i2)', iostat=ios) poldeg
        if (ios /= 0) stop "Read error in file unit iuf"
        !
        ! Read the tolerance for the Gauss-Jordan method
        !
        read(iuf,*)
        read(unit=iuf, fmt=*, iostat=ios) tolerance
        if (ios /= 0) stop "Read error in file unit iuf"
        !
        ! Close files
        !
        close(iuf)
        close(datauf)
        !
        return
    end subroutine read_input 

    subroutine print_input
        !
        ! Subroutine to print input
        !
        implicit none
        !
        write(uf,'(a)') '+---------------------------+'
        write(uf,'(a)') '|           INPUT           |'
        write(uf,'(a)') '+---------------------------+'
        write(uf,*)
        !
        write(uf,'(2a)') 'Data file name: ', data_file_name
        write(uf,*)
        !
        write(uf,'(2a)') 'Set of data points:'
        call write_mat(datmat, uf, fmt, size(datmat, dim=2))         
        !
        write(uf,*)
        write(uf,'(a,e8.2)') 'Tolerance of Gauss-Jordan method: ', tolerance
        !
        return
    end subroutine print_input 

    subroutine write_plot_data
        !
        ! Subroutine to write needed data for the plot
        !
        implicit none
        !
        integer :: i, fu, ios
        character(len=80) :: graph_data_fn 
        !
        ! Write (x, y) coordinates
        !
        open(newunit=fu,file='./data/xy-graph.dat',iostat=ios,action="write")
        if (ios /= 0) stop "Error opening file './data/xy-graph.dat'"
        !
        write(fu,'(a)') 'x y'
        dl1: do i = 1, size(datmat, dim=1)
            !
            write(fu,*) datmat(i,:)
            !
        end do dl1
        !
        close(fu)
        !
        ! Write the coefficients
        !
        open(newunit=fu,file='./data/coeffs-graph.dat',iostat=ios,action="write")
        if (ios /= 0) stop "Error opening file './data/out-graph.dat'"
        !
        write(fu,'(a)') 'i (x**i) ; Polynomial coefficient'
        dl2: do i = 1, size(coefs, dim=1)
            !
            write(fu,*) i-1, ';', coefs(i)
            !
        end do dl2
        !
        close(fu)
        !
        ! Write also the coefficients in separated files as: ex*-graph.dat
        !
        graph_data_fn = data_file_name(1:len(trim(data_file_name))-4) &
        & //'-graph.dat'
        !
        graph_data_fn = trim(graph_data_fn)
        !
        open(newunit=fu,file=graph_data_fn,iostat=ios,action="write")
        if (ios /= 0) stop "Error opening file "//graph_data_fn
        !
        write(fu,'(a)') 'i (x**i) ; Polynomial coefficient'
        dl3: do i = 1, size(coefs, dim=1)
            !
            write(fu,*) i-1, ';', coefs(i)
            !
        end do dl3
        !
        close(fu)
        !
        return
    end subroutine write_plot_data 

    subroutine read_matrix(fu, A, ncol, skip_rows, input_format)
        !
        ! Subroutine to read a matrix from an input file
        !
        implicit none
        integer, intent(in) :: fu, ncol, skip_rows
        character(len=*), intent(in) :: input_format
        character(len=:), allocatable :: actual_format
        real(kind=8), dimension(:,:), allocatable, intent(out) :: A
        !
        ! Dummy variables
        !
        integer :: i, j, ierr, irec, nrow
        character :: charvar
        !
        ! Skip rows
        !
        do i = 1, skip_rows
            read(fu,*)
        end do 
        !
        ! Calculate irec to determine number of rows of 'A' (nrows)
        !
        irec = 1
        do
            read(fu,'(a)',end=100) charvar
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
        rewind(fu)
        !
        ! Allocate matrix
        !
        allocate(A(nrow,ncol), stat=ierr)
        if (ierr .ne. 0) stop 'read_matrix: Error in allocation of A'
        !
        ! Skip rows
        !
        do i = 1, skip_rows
            read(fu,*)
        end do 
        !
        ! Read matrix
        !
        if (input_format == "*") then
            do i = 1, nrow
                read(fu,*) ( A(i,j), j=1, ncol )
            end do
        else
            actual_format = "(*(" // input_format // "))"
            do i = 1, nrow
                read(fu,actual_format) ( A(i,j), j=1, ncol )
            end do
        end if
        !
        return
    end subroutine read_matrix 

    subroutine write_vec(vec, file_unit, input_format)
        !
        ! Prints vector vec (array) in file_unit (integer) with
        ! input_format (string)
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: vec
        integer, intent(in) :: file_unit
        character(len=*), intent(in) :: input_format
        character(len=:), allocatable :: actual_format
        integer :: i, n
        !
        n = size(vec,1)
        !
        if (input_format == "*") then
            do i = 1, n
                write(file_unit,*) vec(i)
            end do
        else
            actual_format = "(*(" // input_format // "))"
            do i = 1, n
                write(file_unit,actual_format) vec(i)
            end do
        end if
        !
        return
    end subroutine write_vec

    subroutine write_mat(A, file_unit, output_format, cpr)
        !
        ! Prints matrix A (array) in file_unit (integer) with
        ! output_format (string). cpr (integer) is the number of columns
        ! printed per row
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
                        write(file_unit,*) (A(i,j),j=mincol,maxcol)
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

end module io 
