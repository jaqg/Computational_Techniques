! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Thursday 17:23:38 10-11-2022 |
! +---------------------------------------------+
subroutine derivatives(xders, yders)
    !
    ! Subroutine to define derivatives of the used function
    !
    use IO, only: themodel, alpha, alphaprime, beta, kappa, kappaprime, lambda
    !
    implicit none
    !
    real(kind=8), dimension(:), allocatable, intent(out) :: xders, yders
    integer :: nders, ierr
    real(kind=8) :: xder1, xder2, xder3, yder1, yder2, yder3
    !
    ! Lotka-Volterra model
    !
    ! dx/dt = f(t,x) ; dy/dt = g(t,y)
    !
    nders = 3 ! Number of terms (y', y'', y''')
    !
    ! Allocate needed arrays
    !
    allocate(xders(nders), stat=ierr)
    if (ierr .ne. 0) stop 'derivatives.f90: Error in allocation of xders'
    !
    allocate(yders(nders), stat=ierr)
    if (ierr .ne. 0) stop 'derivatives.f90: Error in allocation of yders'
    !
    if (themodel == "Simple" .or. themodel == "simple") then
        !
        ! Simple LV
        !
        xder1 = a + b * yn          ! xder1 -> f'(t,x) = df(t,x)/dt
        xders(1) = f(t,xn)          !       -> x'
        xders(2) = f(t,xn) * xder1  !       -> x''
        xders(3) = 0.0_8            !       -> x'''
        !
        yder1 = a + b * xn          ! yder1 -> g'(t,y) = dg(t,y)/dt
        yders(1) = f(t,yn)          !       -> y'
        yders(2) = f(t,yn) * yder1  !       -> y''
        yders(3) = 0.0_8            !       -> y'''
        !
    elseif (themodel == "Logistic" .or. themodel == "logistic") then
        !
        ! Logistic LV
        !
        xder1 = a + 2.0_8 * b * xn + c * yn
        xder2 = 2.0_8 * b
        xders(1) = f(t,xn)
        xders(2) = f(t,xn) * xder1
        xders(3) = f(t,xn) * (xder1**2 + f(t,xn) * xder2)
        !
        yder1 = a + 2.0_8 * b * yn + c * xn
        yder2 = 2.0_8 * b
        yders(1) = f(t,yn)
        yders(2) = f(t,yn) * yder1
        yders(3) = f(t,yn) * (yder1**2 + f(t,yn) * yder2)
        !
    end if
    !
    return
end subroutine derivatives
