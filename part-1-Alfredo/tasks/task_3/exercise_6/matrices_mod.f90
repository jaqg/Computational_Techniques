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
        !
        subroutine write_mat(A, file_unit, input_format, cpr)
            !
            ! Prints matrix A (array) in file_unit (integer) with
            ! input_format (string). cpr (integer) is the number of columns
            ! printed per row
            !
            implicit none
            !
            real(kind=8), dimension(:,:), intent(in) :: A
            integer, intent(in) :: file_unit
            character(len=*), intent(in) :: input_format
            character(len=:), allocatable :: actual_format
            integer :: i, j, k, nrow, ncol, cpr, iter, mincol, maxcol
            !
            nrow = size(A,1)
            ncol = size(A,2)
            !
            iter = ncol/cpr
            !
            if (input_format == "*") then
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
                actual_format = "(*(" // input_format // "))"
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
            n = 0
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
            n = 0
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
            n = 0
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
        subroutine mat_vec_prod(A, v, C)
            !
            ! Matrix by vector product subroutine:
            ! C = Av
            ! C_i = sum_{k=1}^{N} A_{ik} * v_k
            !
            ! where A is a (real) matrix, v a (real) vector and C a (real)
            ! column matrix.
            !
            ! C doesn't need to be allocated in main program as is allocated
            ! in this subroutine
            !
            implicit none
            !
            real(kind=8), dimension(:,:), intent(in) :: A
            real(kind=8), dimension(:), intent(in) :: v
            real(kind=8), dimension(:,:), allocatable, intent(out) :: C
            integer :: i, j, ierr
            integer :: nrowA, ncolA, nv, n
            real(kind=8) :: xtmp
            !
            nrowA = size(A,1)
            ncolA = size(A,2)
            nv = size(v,1)
            !
            ! Check if matrix x vector can actually be multiplied
            !
            n = 0
            if (ncolA .ne. nv) then
                write(11,*) "The matrix and vector can't be multiplied."
                write(11,*) "Check the dimensions."
            else
                n = nrowA
            end if
            !
            allocate(C(n,1), stat=ierr)
            if (ierr .ne. 0) stop 'mat_outer_prod: Error in allocation of C'
            !
            ! Sum of column-by-row outer products
            !
            xtmp = 0.0_8
            do i = 1, nrowA
                do j = 1, ncolA
                    xtmp = xtmp + A(i,j) * v(j)
                end do
                C(i,1) = xtmp
            end do
            !
            return
        end subroutine mat_vec_prod
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
        !
        subroutine scale_diag_mat(A, f, S)
            !
            ! Scale diagonal elements of an input (real) matrix A by a
            ! factor (real) f
            !
            implicit none
            !
            real(kind=8), dimension(:,:), intent(in) :: A
            integer, intent(in) :: f
            real(kind=8), dimension(:,:) :: S
            integer :: i, j, nrowA, ncolA
            !
            nrowA = size(A,1)
            ncolA = size(A,2)
            !
            loop1: do i = 1, nrowA
                loop2: do j = 1, ncolA
                    if (i == j) then ! scale
                        S(i,j) = dble(f) * A(i,j)
                    else ! leave it as it is
                        S(i,j) = A(i,j)
                    end if
                end do loop2
            end do loop1
            !
            return
        end subroutine scale_diag_mat
        !
        real(kind=8) function magn_vec(v)
            !
            ! Function to calculate the magnitude of a (real) vector
            !
            implicit none
            !
            real(kind=8), dimension(:), intent(in) :: v
            integer :: i, n
            real(kind=8) :: suma
            !
            n = size(v,1)
            !
            suma = 0.0_8
            do i = 1, n
                suma = suma + v(i)**2
            end do
            !
            magn_vec = dsqrt(suma)
            !
            return
        end function magn_vec
        !
        function norm_vec(v) result(v_norm)
            !
            ! Function that normalizes to one the vector v (real)
            !
            implicit none
            !
            real(kind=8), dimension(:), intent(in) :: v
            real(kind=8), dimension(:), allocatable :: v_norm
            integer :: n, ierr
            !
            n = size(v,1)
            !
            allocate(v_norm(n), stat=ierr)
            if (ierr .ne. 0) stop 'mat_mod.MOD; func norm_vec: &
                                 & Error in allocation of v_norm'
            !
            !
            v_norm = v/magn_vec(v)
            !
            return
        end function norm_vec
        !
        function norm_mat(A) result(A_norm)
            !
            ! Function that normalizes to one the array A (real) in its
            ! argument list, by columns
            !
            implicit none
            !
            real(kind=8), dimension(:,:), intent(in) :: A
            real(kind=8), dimension(:,:), allocatable :: A_norm
            integer :: j, ierr, nrow, ncol
            !
            nrow = size(A,1)
            ncol = size(A,2)
            !
            allocate(A_norm(nrow,ncol), stat=ierr)
            if (ierr .ne. 0) stop 'mat_mod.MOD; func norm_mat: &
                                 & Error in allocation of A_norm'
            !
            do j = 1, ncol
                A_norm(:,j) = norm_vec(A(:,j))
            end do
            !
            return
        end function norm_mat
end module
