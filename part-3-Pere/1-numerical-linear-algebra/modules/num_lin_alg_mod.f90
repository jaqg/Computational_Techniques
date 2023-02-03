! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Monday 09:29:37 16/01/2023 |
! +-------------------------------------------+
module num_lin_alg_mod

    implicit none

    real(kind=8), dimension(:,:), allocatable :: mat, newmat
    integer :: i, j, ierr

    contains

    subroutine SMUL(m,n,la,A,C)
        !
        ! m:      integer, single precision
        ! n:      integer, single precision
        ! la:     real, double precision
        ! A:      real, double precision array
        ! C:      real, double precision array
        !
        ! m: dim 1 of matrix A
        ! n: dim 2 of matrix A
        ! la: scalar to multiply A
        ! A: matrix to be multiplied
        ! C: output matrix
        !
        implicit none
        !
        integer, intent(in) :: m,n
        real(kind=8), intent(in) :: la
        real(kind=8), dimension(:,:), intent(in) :: A
        real(kind=8),  dimension(:,:), allocatable, intent(out) :: C
        !
        allocate(C(m,n), stat=ierr)
        if (ierr .ne. 0) stop 'main.f90: Error in allocation of C(m,n)'
        !
        dl1: do i = 1, m
            dl2: do j = 1, n
                C(i,j) = A(i,j) * la
            end do dl2 
        end do dl1 
        !
        return
    end subroutine SMUL

    subroutine MADD(m,n,A,B,C)
        !
        ! m:      integer, single precision
        ! n:      integer, single precision
        ! A:      real, double precision array
        ! B:      real, double precision array
        ! C:      real, double precision array
        !
        ! m: dim 1 of matrix C
        ! n: dim 2 of matrix C
        ! A: first matrix to be added
        ! B: second matrix to be added
        ! C: output matrix
        !
        implicit none
        !
        integer, intent(in) :: m,n
        real(kind=8), dimension(:,:), intent(in) :: A, B
        real(kind=8),  dimension(:,:), allocatable, intent(out) :: C
        !
        if (size(A, dim=1) /= size(B, dim=1) .or. &
            & size(A, dim=2) /= size(B, dim=2)) then
            write(unit=*, fmt=*) "MADD: size of matrices doesn't match"
        end if

        allocate(C(m,n), stat=ierr)
        if (ierr .ne. 0) stop 'main.f90: Error in allocation of C(m,n)'
        !
        dl1: do i = 1, m
            dl2: do j = 1, n
                C(i,j) = A(i,j) + B(i,j)
            end do dl2 
        end do dl1 
        !
        return
    end subroutine MADD 

    subroutine TRANS(m,n,A,AT)
        !
        ! m:      integer, single precision
        ! n:      integer, single precision
        ! A:      real, double precision array
        ! AT:     real, double precision array
        !
        ! m: dim 1 of matrix AT
        ! n: dim 2 of matrix AT
        ! A: matrix to be transposed
        ! AT: transposed matrix
        !
        implicit none
        !
        integer, intent(in) :: m,n
        real(kind=8), dimension(:,:), intent(in) :: A
        real(kind=8),  dimension(:,:), allocatable, intent(out) :: AT
        !
        allocate(AT(m,n), stat=ierr)
        if (ierr .ne. 0) stop 'TRANS: Error in allocation of AT(m,n)'
        !
        dl1: do i = 1, m
            dl2: do j = 1, n
                AT(i,j) = A(j,i)
            end do dl2 
        end do dl1 
        !
        return
    end subroutine TRANS 
end module num_lin_alg_mod
