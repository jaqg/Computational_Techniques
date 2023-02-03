! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 19:28:39 05-11-2022 |
! +---------------------------------------------+

module interface_block
    !
    ! Module to source interface of external functions/subroutines
    !
    implicit none
    !
    interface
        !
        function func(t, y) result(res)
            implicit none
            real(kind=8), optional, intent(in) :: t
            real(kind=8), dimension(:), intent(in) :: y
            real(kind=8), dimension(:), allocatable :: res
        end function func
        !
        subroutine methods(f, y0, t0, tf, h, t, y)
            implicit none
            procedure(func) :: f
            real(kind=8), dimension(:), intent(in) :: y0
            real(kind=8), intent(in) :: t0, tf, h
            real(kind=8), dimension(:), allocatable, intent(out) :: t
            real(kind=8), dimension(:,:), allocatable, intent(out) :: y
        end subroutine methods
        !
        subroutine methods2(f, y0, t0, tf, h, threshold, t, y)
            implicit none
            procedure(func) :: f
            real(kind=8), dimension(:), intent(in) :: y0
            real(kind=8), intent(in) :: t0, tf, h, threshold
            real(kind=8), dimension(:), allocatable, intent(out) :: t
            real(kind=8), dimension(:,:), allocatable, intent(out) :: y
        end subroutine methods2
        !
        subroutine error(x, y, theerr)
            implicit none
            real(kind=8), dimension(:,:), allocatable, intent(in) :: x, y
            real(kind=8), dimension(:,:), allocatable, intent(out) :: theerr
        end subroutine error
        !
        subroutine check_neg_prob(y, n)
            implicit none
            real(kind=8), dimension(:,:), intent(in) :: y
            integer(kind=8), intent(out) :: n
        end subroutine check_neg_prob
        !
    end interface
    !
end module
