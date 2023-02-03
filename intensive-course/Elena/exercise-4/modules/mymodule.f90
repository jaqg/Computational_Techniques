! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 00:48:51 18-12-2022 |
! +-------------------------------------------+

module mymodule
    !
    ! Module to contain my functions/subrtouines
    !
    implicit none
    !
    ! Subprograms
    !
    contains
    real(kind=8) function f(x,y)
        !
        ! Function to minimize
        !
        ! f     real, double precision
        ! x     real, double precision
        ! y     real, double precision
        !
        ! f: result of the function
        ! x: independent variable
        ! y: dependent variable
        !
        implicit none
        real(kind=8), intent(in) :: x, y
        !
        ! f(x,y) = 25x^2 + y^2
        !
        f = 25.0_8 * x**2 + y**2
        !
        return
    end function f
    !
    real(kind=8) function binomial_coeff(n, m)
        !
        ! Function to calculate the binomial coefficient (n m)
        !
        ! binomial_coeff        real, double precision
        ! n                     innteger, single precision
        ! m                     innteger, single precision
        !
        ! binomial_coeff: result of the function; binomial coeficient (n m)
        ! n: fixed set of elements
        ! m: element to choose from n
        !
        implicit none
        integer, intent(in) :: n, m
        !
        ! Note: since F2008, n! = factorial(n) = Gamma(n+1)
        !
        ! bin(n, m) = n!/(m! (n - m)!)
        !
        binomial_coeff = Gamma( dble(n+1) ) / &
        & ( Gamma( dble(m+1) ) * Gamma( dble(n - m + 1) ) )
        !
        return
    end function binomial_coeff
    !
    subroutine finite_diff(method, n, x, y, h, resx, resy)
        !
        ! Subroutine of the finite difference method
        !
        ! method        character
        ! n             integer, single precision
        ! x             real, double precision
        ! y             real, double precision
        ! h             real, double precision
        ! resx          real, double precision
        ! resy          real, double precision
        !
        ! method: method for finite difference (forward, backward, central)
        ! n: n-th derivative f^{(n)'}
        ! x: x point to evaluate f^{(n)'}
        ! y: y point to evaluate f^{(n)'}
        ! h: spacing
        ! resx: result of the subroutine; f^{(n)'}(x)
        ! resy: result of the subroutine; f^{(n)'}(y)
        !
        implicit none
        !
        character(len=*), intent(in) :: method
        integer, intent(in) :: n
        real(kind=8), intent(in) :: x, y, h
        real(kind=8), intent(out) :: resx, resy
        !
        ! Dummy variable
        !
        integer :: i
        !
        ! Finite diffference for each method
        !
        if (method=='Forward' .or. method=='forward') then
            !
            ! Forward method
            !
            ! f^{(n)'} (x) = 1/h^n sum_{i=0}^n (-1)^{n-i} bin(n i) f(x+ih, y)
            !
            resx = 0.0_8
            !
            fdl1: do i = 0, n
                !
                resx = resx + &
                & (-1.0_8)**(n-i) * binomial_coeff(n,i) * f(x + dble(i)*h, y)
                !
            end do fdl1
            !
            resx = resx/h**n
            !
            ! f^{(n)'} (y) = 1/h^n sum_{i=0}^n (-1)^{n-i} bin(n i) f(x, y+ih)
            !
            resy = 0.0_8
            !
            fdl2: do i = 0, n
                !
                resy = resy + &
                & (-1.0_8)**(n-i) * binomial_coeff(n,i) * f(x, y + dble(i) * h)
                !
            end do fdl2
            !
            resy = resy/h**n
            !
        elseif (method=='Backward' .or. method=='backward') then
            !
            ! Backward method
            !
            ! f^{(n)'} (x) = 1/h^n sum_{i=0}^n (-1)^i bin(n i) f(x-ih, y)
            !
            resx = 0.0
            bdl1: do i = 0, n
                !
                resx = resx + &
                & (-1.0_8)**i * binomial_coeff(n,i) * f(x - dble(i)*h, y)
                !
            end do bdl1
            !
            resx = resx/h**n
            !
            ! f^{(n)'} (y) = 1/h^n sum_{i=0}^n (-1)^i bin(n i) f(x, y-ih)
            !
            resy = 0.0_8
            bdl2: do i = 0, n
                !
                resy = resy + &
                & (-1.0_8)**i * binomial_coeff(n,i) * f(x, y - dble(i) * h)
                !
            end do bdl2
            !
            resy = resy/h**n
            !
        elseif (method=='Central' .or. method=='central') then
            !
            ! Central method
            !
            ! f^{(n)'}(x) = 1/h^n sum_{i=0}^n (-1)^i bin(n i) f(x+(n/2-i)h, y)
            !
            resx = 0.0_8
            cdl1: do i = 0, n
                !
                resx = resx + &
                & (-1.0_8)**i * binomial_coeff(n,i) * &
                & f(x + (dble(n)/2.0_8 - dble(i)) * h, y)
                !
            end do cdl1
            !
            resx = resx/h**n
            !
            ! f^{(n)'}(y) = 1/h^n sum_{i=0}^n (-1)^i bin(n i) f(x, y+(n/2-i)h)
            !
            resy = 0.0_8
            cdl2: do i = 0, n
                !
                resy = resy + &
                & (-1.0_8)**i * binomial_coeff(n,i) * &
                & f(x, y + (dble(n)/2.0_8 - dble(i)) * h)
                !
            end do cdl2
            !
            resy = resy/h**n
            !
        else
            write(unit=*, fmt=*) 'ERROR finite_diff: bad method input'
        end if
        !
        return
    end subroutine finite_diff
    !
    ! subroutine Newton_Raphson(method, x0, h, threshold, totiter, xmin)
    !     !
    !     ! Subroutine for the Newton-Raphson method
    !     !
    !     ! method        character
    !     ! x0            real, double precision
    !     ! h             real, double precision
    !     ! threshold     real, double precision
    !     ! totiter       integer, single precision
    !     ! xmin          real, double precision
    !     !
    !     ! method: method for the finite difference
    !     ! x0: initial point x_n = x_0
    !     ! h: spacing for the finite difference
    !     ! threshold: threshold for convergence
    !     ! totiter: total number of iterations
    !     ! xmin: result of the subroutine; position of the minimum
    !     !
    !     implicit none
    !     !
    !     character(len=*) :: method
    !     real(kind=8), intent(in) :: x0, h, threshold
    !     integer, intent(out) :: totiter
    !     real(kind=8), intent(out) :: xmin
    !     !
    !     ! Dummy variables
    !     !
    !     real(kind=8) :: xn, xnm1, xnp1, first_diff, second_diff
    !     !
    !     ! 0) Initialize dummy variables
    !     !
    !     totiter = 0
    !     xnm1 = x0 + 2.0_8 * threshold   ! random number > threshold
    !     !
    !     ! 1) Choose an initial point x_n = x_0
    !     !
    !     xn = x0
    !     !
    !     ! 2) Evaluate f'(x_n)
    !     !
    !     2 continue
    !     !
    !     totiter = totiter + 1
    !     !
    !     call finite_diff(method, 1, xn, h, first_diff)
    !     !
    !     write(*,'(i0, 2x, 3(f15.8))') totiter, xn, f(xn), first_diff
    !     !
    !     ! 3) IF f'(x_n) approx. 0 and x_n - x_{n-1} approx. 0 GO TO step 5
    !     !
    !     if (abs(first_diff)<threshold .and. abs(xn - xnm1)<threshold) then
    !         goto 5
    !     else
    !         !
    !         ! 4) ELSE (f'(x_n) /= 0) THEN
    !         !        Compute f''(x_n)
    !         !        Compute next point;
    !         !        x_{n+1} = x_n - f'(x_n)/f''(x_n) & f(x_{n+1}) < f(x_n)
    !         !        GO TO step 2
    !         !
    !         call finite_diff(method, 2, xn, h, second_diff)
    !         !
    !         xnp1 = xn - first_diff/second_diff
    !         !
    !         if (f(xnp1) < f(xn)) then
    !             !
    !             xnm1 = xn
    !             xn = xnp1
    !             !
    !             goto 2
    !             !
    !         end if
    !         !
    !     end if
    !     !
    !     ! 5) END program
    !     !
    !     5 continue
    !     !
    !     xmin = xn
    !     !
    !     return
    ! end subroutine Newton_Raphson
end module
