module mat_mod
    implicit none
    contains
        subroutine read_mat(A, file_unit, input_format)
            !
            ! Reads matrix A in file_unit (integer) with input_format (string)
            !
            implicit none
            !
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
            ! Prints matrix A in file_unit (integer) with input_format (string)
            !
            implicit none
            !
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
            ! C doesn't need to be allocated in main program as is allocated
            ! in this subroutine
            !
            implicit none
            !
            real(kind=8), dimension(:,:), intent(in) :: A, B
            real(kind=8), dimension(:,:), allocatable :: C
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
            if (ierr .ne. 0) stop 'matrix_mult: Error in allocation of C'
            !
            ! Product of matrices
            !
            C = 0.0_8
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
        subroutine vec_outer_prod(a, b, C)
            !
            ! Outer product of two (real) vectors a, b resulting in
            ! a (real) matrix C
            !
            implicit none
            !
            real(kind=8), dimension(:), intent(in) :: a, b
            real(kind=8), dimension(:,:), allocatable, intent(out) :: C
            integer :: i, j, na, nb, n, ierr
            !
            na = size(a,1)
            nb = size(b,1)
            !
            ! Check if vectors can actually be multiplied
            !
            if (nb .ne. na) then
                write(11,*) "The vectors can't be multiplied."
                write(11,*) "Check the size of the vectors."
            else
                n = na
            end if
            !
            allocate(C(n,n), stat=ierr)
            if (ierr .ne. 0) stop 'vec_outer_prod: Error in allocation of C'
            !
            C = 0.0_8
            loop1: do j = 1, n
                loop2: do i = 1, n
                    C(i,j) = a(i) * b(j)
                end do loop2
            end do loop1
            !
            return
        end subroutine vec_outer_prod
        !
        subroutine mat_outer_prod(A, B, C)
            !
            ! Matrix outer product subroutine:
            ! C = AB = sum_{k=1}^{N} a_k^{col} b_k^{row}
            ! where a_K^{col} are column vectors of matrix A,
            ! and b_k^{row} are row vectors of matrix B
            !
            ! C doesn't need to be allocated in main program as is allocated
            ! in this subroutine
            !
            implicit none
            !
            real(kind=8), dimension(:,:), intent(in) :: A, B
            real(kind=8), dimension(:,:), allocatable, intent(out) :: C
            real(kind=8), dimension(:,:), allocatable :: Cvop
            integer :: i, ierr
            integer :: nrowA, ncolA, nrowB, ncolB, n
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
            if (ierr .ne. 0) stop 'mat_outer_prod: Error in allocation of C'
            !
            allocate(Cvop(n,n), stat=ierr)
            if (ierr .ne. 0) stop 'mat_outer_prod: Error in allocation of Cvop'
            !
            ! Sum of column-by-row outer products
            !
            C = 0.0_8
            loop1: do i = 1, n
                call vec_outer_prod(A(:,i), B(i,:), Cvop)
                C = C + Cvop
            end do loop1
            !
            return
        end subroutine mat_outer_prod
        !
        subroutine sym_mat(A, S)
            !
            ! Symmetrize a matrix A, resulting in S, as
            ! S = 1/2 (A + A^T)
            !
            ! S needs to be allocated in the main program
            !
            implicit none
            !
            real(kind=8), dimension(:,:), intent(in) :: A
            real(kind=8), dimension(:,:), intent(out) :: S
            integer :: i, j, nrowA, ncolA
            !
            nrowA = size(A,1)
            ncolA = size(A,2)
            !
            loop1: do i = 1, nrowA
                loop2: do j = 1, ncolA
                    S(i,j) = 1.0_8/2.0_8 * (A(i,j) + A(j,i))
                end do loop2
            end do loop1
            !
            return
        end subroutine sym_mat
end module
