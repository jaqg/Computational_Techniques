! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 19:39:32 23-11-2022 |
! +----------------------------------------------+
subroutine check_neg_prob(y, n)
    !
    ! Subroutine to check if any value of y becomes <0
    !
    implicit none
    !
    real(kind=8), dimension(:,:), intent(in) :: y
    integer(kind=8), intent(out) :: n
    !
    ! Dummy variables
    !
    integer :: i, j
    !
    cl1: do i = 1, size(y,1)
        !
        do j = 1, size(y,2)
            if ( y(i,j) < 0 ) then
                write(*,'(2(a,i0),a)') 'i = ', i, ', j = ', j, ', y<0, exiting'
                exit cl1
            end if
        end do
        !
        n = i
        !
    end do cl1
    !
    return
end subroutine check_neg_prob
