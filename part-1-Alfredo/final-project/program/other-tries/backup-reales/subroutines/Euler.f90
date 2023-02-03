! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 23:16:48 05-11-2022 |
! +---------------------------------------------+
subroutine Euler(xn, yn, xnp1, ynp1)
    !
    ! Euler's method:
    ! y_{n+1} = y_n + h * f(t_n, y_n)
    !
    use IO, only: h, alpha, alphaprime, beta, kappa, kappaprime, lambda
    !
    implicit none
    !
    real(kind=8), intent(in) :: xn, yn
    real(kind=8), intent(out) :: xnp1, ynp1
    !
    interface
        real(kind=8) function LV(c1, c1prime, c2, p1, p2)
            implicit none
            real(kind=8), intent(in) :: c1, c1prime, c2, p1, p2
        end function LV
    end interface
    !
    xnp1 = xn + h * LV(-alpha, alphaprime, beta, xn, yn)
    ynp1 = yn + h * LV(kappa, -kappaprime, -lambda, yn, xn)
    !
    return
end subroutine Euler
