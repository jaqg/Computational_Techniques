! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 15:45:39 09-11-2022 |
! +----------------------------------------------+
subroutine RK4(xn, yn, xnp1, ynp1)
    !
    ! Runge-Kutta's 4th order method:
    ! y_{k+1} = y_k + 1/6 * ( K_1 + 2K_2 + 2K_3 + K_4 )
    !
    use IO, only: h, alpha, alphaprime, beta, kappa, kappaprime, lambda
    !
    implicit none
    !
    real(kind=8), intent(in) :: xn, yn
    real(kind=8), intent(out) :: xnp1, ynp1
    real(kind=8) :: Kx1, Kx2, Kx3, Kx4, Ky1, Ky2, Ky3, Ky4
    !
    interface
        real(kind=8) function LV(c1, c1prime, c2, p1, p2)
            implicit none
            real(kind=8), intent(in) :: c1, c1prime, c2, p1, p2
        end function LV
    end interface
    !
    ! Constants K1, K2, K3, K4
    !
    Kx1 = h * LV(-alpha, alphaprime, beta, xn, yn)
    Ky1 = h * LV(kappa, -kappaprime, -lambda, yn, xn)
    !
    Kx2 = h * LV(-alpha,alphaprime,beta, xn + (Kx1/2.0_8), yn + (Ky1/2.0_8))
    Ky2 = h * LV(kappa,-kappaprime,-lambda, yn + (Ky1/2.0_8), xn + (Kx1/2.0_8))
    !
    Kx3 = h * LV(-alpha,alphaprime,beta, xn + (Kx2/2.0_8), yn + (Ky2/2.0_8))
    Ky3 = h * LV(kappa,-kappaprime,-lambda, yn + (Ky2/2.0_8), xn + (Kx2/2.0_8))
    !
    Kx4 = h * LV(-alpha, alphaprime, beta, xn + Kx3, yn + Ky3)
    Ky4 = h * LV(kappa, -kappaprime, -lambda, yn + Ky3, xn + Kx3)
    !
    ! Final results
    !
    xnp1 = xn + 1.0_8/6.0_8 * ( Kx1 + 2.0_8 * Kx2 + 2.0_8 * Kx3 + Kx4 )
    ynp1 = yn + 1.0_8/6.0_8 * ( Ky1 + 2.0_8 * Ky2 + 2.0_8 * Ky3 + Ky4 )
    !
    return
end subroutine RK4
