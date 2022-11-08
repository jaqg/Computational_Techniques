! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 23:16:48 05-11-2022 |
! +---------------------------------------------+
subroutine Euler(yn, h, f, c1, c1prime, c2, p1, p2, ynp1)
    !
    ! Euler's method:
    ! y_{n+1} = y_n + h * f(t_n, y_n)
    !
    implicit none
    !
    real(kind=8), intent(in) :: yn, h, c1, c1prime, c2, p1, p2
    real(kind=8), intent(out) :: ynp1
    !
    interface
        real(kind=8) function f(c1, c1prime, c2, p1, p2)
            implicit none
            real(kind=8), intent(in) :: c1, c1prime, c2, p1, p2
        end function f
    end interface
    !
    ynp1 = yn + h * f(c1, c1prime, c2, p1, p2)
    !
    return
end subroutine Euler
