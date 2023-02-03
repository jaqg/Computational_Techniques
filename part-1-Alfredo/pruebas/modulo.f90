module modulo1
    implicit none
    contains
        subroutine read_mat(A, file_unit, input_format)
            !
            ! Reads matrix A in file_unit with the input_format (string)
            !
            implicit none
            real(kind=8), dimension(:,:) :: A
            integer, intent(in) :: file_unit
            character(len=*), intent(in) :: input_format
            character(len=:), allocatable :: actual_format
            integer :: i, j, nrow, ncol
            !
            nrow = size(A,1)
            ncol = size(A,2)
            !
            if (input_format == "*") then
                do i = 1, nrow
                    read(file_unit,*) ( A(i,j), j=1, ncol )
                end do
            else
                actual_format = "(*(" // input_format // "))"
                do i = 1, nrow
                    read(file_unit,actual_format) ( A(i,j), j=1, ncol )
                end do
            end if
            !
            return
        end subroutine read_mat
        !
        subroutine write_mat(A, file_unit, input_format)
            !
            ! Prints matrix A in file_unit with the input_format (string)
            !
            implicit none
            real(kind=8), dimension(:,:), intent(in) :: A
            integer, intent(in) :: file_unit
            character(len=*), intent(in) :: input_format
            character(len=:), allocatable :: actual_format
            integer :: i, j, nrow, ncol
            !
            nrow = size(A,1)
            ncol = size(A,2)
            !
            if (input_format == "*") then
                do i = 1, nrow
                    write(file_unit,*) ( A(i,j), j=1, ncol )
                end do
            else
                actual_format = "(*(" // input_format // "))"
                do i = 1, nrow
                    write(file_unit,actual_format) ( A(i,j), j=1, ncol )
                end do
            end if
            !
            return
        end subroutine write_mat
        !
        subroutine matrix_mult(A, B, C)
            !
            ! Matrix multiplication subroutine:
            ! C = AB
            !
            implicit none
            real(kind=8), dimension(:,:), intent(in) :: A, B
            real(kind=8), dimension(:,:), allocatable, intent(out) :: C
            integer :: i, j, k, ierr
            integer :: nrowA, ncolA, nrowB, ncolB, n
            real(kind=8) :: xtmp
            !
            nrowA = size(A,1)
            ncolA = size(A,2)
            nrowB = size(B,1)
            ncolB = size(B,2)
            !
            ! Check if matrices can actually be multiplied
            !
            if (nrowB .ne. ncolA) then
                write(11,*) "The matrices can't be multiplied."
                write(11,*) "Check the dimension of the matrices."
            else
                n = nrowA
            end if
            !
            allocate(C(n,n), stat=ierr)
            if (ierr .ne. 0) stop 'Error in allocation of C'
            !
            ! Product of matrices
            !
            loop1: do j = 1, n
                loop2: do i = 1, n
                    xtmp = 0.0_8
                    loop3: do k = 1, ncolA
                        xtmp = xtmp + A(i,k) * B(k,j)
                    end do loop3
                    C(i,j) = xtmp
                end do loop2
            end do loop1
            !
            return
        end subroutine matrix_mult
        !
        subroutine vec_mult(a, b, C)
            !
            ! Vector multiplication subroutine:
            ! C = ab
            !
            implicit none
            real(kind=8), dimension(:), intent(in) :: a, b
            real(kind=8), dimension(:,:), allocatable, intent(out) :: C
            integer :: i, j, ierr
            integer :: na, nb, n
            !
            na = size(a,1)
            nb = size(b,1)
            write(*,*) 'na =', na, 'nb =', nb
            write(*,*)
            !
            ! Check if matrices can actually be multiplied
            !
            if (nb .ne. na) then
                write(11,*) "The vectors can't be multiplied."
                write(11,*) "Check the dimension of the vectors."
            else
                n = na
            end if
            write(*,*) 'n =', n
            write(*,*)
            !
            allocate(C(n,n), stat=ierr)
            if (ierr .ne. 0) stop 'Error in allocation of C'
            !
            ! Product of matrices
            !
            loop1: do j = 1, n
                loop2: do i = 1, n
                    C(i,j) = a(i) * b(j)
                end do loop2
            end do loop1
            !
            do i = 1, n
                write(*,*) C(i,:)
            end do
            !
            return
        end subroutine vec_mult
end module
