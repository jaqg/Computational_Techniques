! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 20:18:30 01/03/2023 |
! +----------------------------------------------+

module gauss_jordan
    !
    ! Subroutine for the Gauss-Jordan method
    !
    implicit none
    !
    contains

    subroutine gauss_elimination(A, tolerance)
        !
        ! Subroutine for the Gauss elimination with partial pivoting
        !
        implicit none
        !
        real(kind=8), intent(in) :: tolerance 
        real(kind=8), dimension(:,:), intent(inout) :: A
        ! Dummy variables
        integer :: i, j, ncol, nrow, maxelem, ierr
        real(kind=8), dimension(:), allocatable :: tmp 
        !
        ! Set number of rows and columns
        !
        nrow = size(A, dim=1)
        ncol = size(A, dim=2)
        !
        ! Allocate temporal array
        !
        allocate(tmp(nrow), stat=ierr)
        if (ierr .ne. 0) stop 'gauss_elimination: Error in allocation of tmp'
        !
        ! Main loop over columns of A
        !
        ml1: do i = 1, nrow
            ! write(*,*) 'ITER =', i
            !
            ! Check if the pivot = 0
            !   if (pivot = 0) swap rows (partial pivoting)
            !   else continue
            !
            if (dabs(A(i,i)) .lt. tolerance) then
                !
                ! Swap the row A(i,:) with the row which element A(j,i) (j/=i)
                ! is the largest absolute value of the column; |A(:,i)|
                !
                ! Look for the largest abs. value
                !
                maxelem = maxloc( dabs(A(:,i)), 1 )
                !
                write(*,'(a,2(i0,a),f10.4)') 'Largest abs. val. element: A(', &
                & maxelem,',',i,') =', A(maxelem,i)
                write(*,*)
                !
                ! Swap rows
                !
                tmp(:) = A(i,:)
                A(i,:) = A(maxelem,:)
                A(maxelem,:) = tmp(:)
                !
            end if
            !
            ! Make the pivot = 1 => A(i,:)/A(i,i)
            !
            A(i,:) = A(i,:)/A(i,i)
            !
            ! Make A(j,i) = 0 for j /= i => A(j,:) - ( A(j,i)/A(i,i) ) * A(i,:)
            !
            ml2: do j = i+1, nrow
                A(j,:) = A(j,:) - ( A(j,i)/A(i,i) ) * A(i,:)
            end do ml2
            !
            ! write(*,*) 'A ='
            ! do j = 1, nrow
            !     write(*,'(*(f10.2))') ( A(j,k), k = 1, ncol )
            ! end do 
            ! write(*,*)
            !
        end do ml1
        !
        ! Check if the system is singular
        !
        if ( dabs(A(nrow,ncol)) .lt. tolerance ) then
            write(*,*) 'The system is singular'
        end if
        !
        return
    end subroutine gauss_elimination 

    subroutine jordan(A, c)
        !
        ! Subroutine to reduce a triangular matrix to a diagonal one by the
        ! jordan method
        !
        implicit none
        !
        real(kind=8), dimension(:,:), intent(inout) :: A
        real(kind=8), dimension(:), allocatable, intent(out) :: c
        ! Dummy variables
        integer :: i, j, ierr
        !
        ! Allocate needed arrays
        !
        allocate(c(size(A, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'jordan: Error in allocation of c'
        !
        ! Make A diagonal
        !
        do i=size(A, dim=1), 1, -1
            do j= i-1, 1, -1
                A(j,:) = A(j,:) - (A(j,i))* (A(i,:))
            end do
        end do
        !
        ! Store coefficients in array c
        !
        c = A(:, size(A, dim=2))
        !
        return
    end subroutine jordan 

    subroutine back_substitution(A, b, x)
        !
        ! Subroutine to perform the backsubstitution
        !
        implicit none
        !
        real(kind=8), dimension(:,:), intent(in) :: A
        real(kind=8), dimension(:), intent(in) :: b
        real(kind=8), dimension(:), allocatable, intent(out) :: x
        ! Dummy variables
        integer :: i, j, n, ierr
        real(kind=8) :: suma 
        !
        ! Check that A and b are the same size
        !
        n = size(A, dim=1)
        !
        if (n .ne. size(b, dim=1)) stop &
            & 'ERROR back_subtitution: A and b are not the same size'
        !
        ! Allocate array x
        !
        allocate(x(n), stat=ierr)
        if (ierr .ne. 0) stop 'back_substitution: Error in allocation of x'
        !
        ! Compute each element of x as
        !
        ! x_i = 1/a_{ii} [ b_i - sum_{j=i+1}^N a_{ij} x_j ]
        !
        xl1: do i = n, 1, -1
            !
            ! Compute first the sum
            !
            suma = 0.0_8
            !
            xl2: do j = i+1, n
                suma = suma + A(i,j) * x(j)
            end do xl2
            !
            x(i) = 1.0_8/A(i,i) * ( b(i) - suma )
            !
        end do xl1
        !
        return
    end subroutine back_substitution 

    ! subroutine inv_mat_gauss(A, tolerance, Ainv)
    !     !
    !     ! Subroutine to compute the inverse of matrix A with Gauss-Jordan
    !     ! elimination
    !     !
    !     ! [A|I] -> [I|A^{-1}]
    !     !
    !     implicit none
    !     !
    !     real(kind=8), dimension(:,:), intent(in) :: A
    !     real(kind=8), intent(in) :: tolerance 
    !     real(kind=8), dimension(size(A, 1), size(A, 2)), intent(out) :: Ainv
    !     ! Dummy variables
    !     integer :: i, j, nrow, ncol 
    !     real(kind=8), dimension(size(A, 1), 2*size(A, 2)) :: tmp 
    !     !
    !     ! Get number of rows and columns of array A
    !     !
    !     nrow = size(A, dim=1)
    !     ncol = size(A, dim=2)
    !     !
    !     ! Store input matrix A into a temporal array, tmp
    !     !
    !     dlA1: do i = 1, ncol
    !         tmp(:,i) = A(:,i)
    !     end do dlA1
    !     !
    !     ! Stick identity matrix to the right of A
    !     !
    !     dlt1: do i = 1, ncol
    !         dlt2: do j = 1, nrow
    !             if (i .eq. j) then 
    !                 tmp(j,i+ncol) = 1.0_8
    !             else
    !                 tmp(j,i+ncol) = 0.0_8
    !             end if
    !         end do dlt2
    !     end do dlt1
    !     !
    !     ! Perform Gauss-Jordan elimination to A
    !     !
    !     call gauss_elimination(tmp, tolerance)
    !     !
    !     write(*,*) 'tmp ='
    !     do i = 1, nrow
    !         write(*,'(*(f10.2))') ( tmp(i,j), j = 1, 2*ncol )
    !     end do 
    !     !
    !     return
    ! end subroutine inv_mat_gauss 

end module
