module matmul_mod
    implicit none
    contains
        subroutine matrix_mult(A, B, C)
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
end module
