! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 23:45:16 05-11-2022 |
! +---------------------------------------------+
subroutine modEuler(x0, y0, h, threshold, resx, resy)
    !
    ! Modified Euler's method with the trapezoid method:
    ! y_k^{(n+1)} = y_0 + h/2 * ( f(t_0, y_0) + f(t_k, y_k^{(n)}) )
    !
    implicit none
    !
    real(kind=8) :: x0, y0, h, threshold
    real(kind=8) :: xki, xkip1, yki, ykip1
    real(kind=8), intent(out) :: resx, resy
    real(kind=8) :: alpha, alphaprime, beta, kappa, kappaprime, lambda
    real(kind=8) :: f0, g0, fki, gki
    real(kind=8) :: errx, erry
    !
    interface
        real(kind=8) function LV(c1, c1prime, c2, p1, p2)
            implicit none
            real(kind=8), intent(in) :: c1, c1prime, c2, p1, p2
        end function LV
    end interface
    !
    ! La subrutina sera llamada a cada tiempo t_k (a cada indice k), por lo que
    ! dentro de esta subrutina hay que loopear sobre n hasta un threshold dado
    !
    !
    ! f0 = dx/dt |_{t=t_0} ; g0 = dy/dt |_{t=t_0}
    !
    f0 = LV(-alpha, alphaprime, beta, x0, y0)
    g0 = LV(kappa, -kappaprime, -lambda, y0, x0)
    !
    ! xk(1) -> x_k^{(0)} = x0 + h * f(t_0, x_0)
    ! yk(1) -> y_k^{(0)} = y0 + h * f(t_0, y_0)
    !
    xki = x0 + h * f0
    yki = y0 + h * g0
    !
    ml: do
        !
        ! fk(n) -> f_k^{(n)} = f(x_k^{(n)},y_k^{(n)})
        ! gk(n) -> g_k^{(n)} = g(x_k^{(n)},y_k^{(n)})
        !
        fki = LV(-alpha, alphaprime, beta, xki, yki)
        gki = LV(kappa, -kappaprime, -lambda, yki, xki)
        !
        ! x_k^{(n+1)} = x_0 + h/2 * ( f(t_0, x_0) + f(t_k, x_k^{(n)}) )
        ! y_k^{(n+1)} = y_0 + h/2 * ( f(t_0, y_0) + f(t_k, y_k^{(n)}) )
        !
        xkip1 = x0 + h/2.0_8 * ( f0 + fki )
        ykip1 = y0 + h/2.0_8 * ( g0 + gki )
        !
        ! Check for convergence
        !
        errx = abs( xkip1 - xki )
        erry = abs( ykip1 - yki )
        !
        if (errx < threshold .and. erry < threshold) then
            resx = xkip1
            resy = ykip1
            exit
        end if
        !
        xki = xkip1
        yki = ykip1
        !
    end do ml
    !
    return
end subroutine modEuler
