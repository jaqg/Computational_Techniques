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
        real(kind=8) function f(x)
            !
            ! Function to find the minimum of
            !
            ! f     real, double precision
            ! x     real, double precision
            !
            ! f: result of the function
            ! x: independent variable
            !
            implicit none
            real(kind=8), intent(in) :: x
            !
            ! f = x^2 - x
            !
            f = x**2 - x
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
        subroutine finite_diff(method, n, x, h, res)
            !
            ! Subroutine of the finite difference method
            !
            ! method        character
            ! n             integer, single precision
            ! x             real, double precision
            ! h             real, double precision
            ! res           real, double precision
            !
            ! method: method for finite difference (forward, backward, central)
            ! n: n-th derivative f^{(n)'}
            ! x: point to evaluate f^{(n)'}
            ! h: spacing
            ! res: result of the subroutine; f^{(n)'}(x)
            !
            implicit none
            !
            character(len=*), intent(in) :: method
            integer, intent(in) :: n
            real(kind=8), intent(in) :: x, h
            real(kind=8), intent(out) :: res
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
                ! f^{(n)'} (x) = 1/h^n sum_{i=0}^n (-1)^{n-i} bin(n i) f(x+ih)
                !
                res = 0.0
                fdl1: do i = 0, n
                    !
                    res = res + &
                     & (-1.0_8)**(n-i) * binomial_coeff(n,i) * f(x + dble(i)*h)
                    !
                end do fdl1
                !
                res = res/h**n
                !
            elseif (method=='Backward' .or. method=='backward') then
                !
                ! Backward method
                !
                ! f^{(n)'} (x) = 1/h^n sum_{i=0}^n (-1)^i bin(n i) f(x-ih)
                !
                res = 0.0
                bdl1: do i = 0, n
                    !
                    res = res + &
                     & (-1.0_8)**i * binomial_coeff(n,i) * f(x - dble(i)*h)
                    !
                end do bdl1
                !
                res = res/h**n
                !
            elseif (method=='Central' .or. method=='central') then
                !
                ! Central method
                !
                ! f^{(n)'}(x) = 1/h^n sum_{i=0}^n (-1)^i bin(n i) f(x+(n/2-i)h)
                !
                res = 0.0
                cdl1: do i = 0, n
                    !
                    res = res + &
                     & (-1.0_8)**i * binomial_coeff(n,i) * &
                     & f(x + (dble(n)/2.0_8 - dble(i)) * h)
                    !
                end do cdl1
                !
                res = res/h**n
                !
            else
            end if
            !
            return
        end subroutine finite_diff
        !
        subroutine Newton_Raphson(method, x0, h, threshold, totiter, xmin)
            !
            ! Subroutine for the Newton-Raphson method
            !
            ! method        character
            ! x0            real, double precision
            ! h             real, double precision
            ! threshold     real, double precision
            ! totiter       integer, single precision
            ! xmin          real, double precision
            !
            ! method: method for the finite difference
            ! x0: initial point x_n = x_0
            ! h: spacing for the finite difference
            ! threshold: threshold for convergence
            ! totiter: total number of iterations
            ! xmin: result of the subroutine; position of the minimum
            !
            implicit none
            !
            character(len=*) :: method
            real(kind=8), intent(in) :: x0, h, threshold
            integer, intent(out) :: totiter
            real(kind=8), intent(out) :: xmin
            !
            ! Dummy variables
            !
            real(kind=8) :: xn, xnm1, xnp1, first_diff, second_diff
            !
            ! 0) Initialize dummy variables
            !
            totiter = 0
            xnm1 = x0 + 2.0_8 * threshold   ! random number > threshold
            !
            ! 1) Choose an initial point x_n = x_0
            !
            xn = x0
            !
            ! 2) Evaluate f'(x_n)
            !
            2 continue
            !
            totiter = totiter + 1
            !
            call finite_diff(method, 1, xn, h, first_diff)
            !
            write(*,'(i0, 2x, 3(f20.14))') totiter, xn, f(xn), first_diff
            !
            ! 3) IF f'(x_n) approx. 0 and x_n - x_{n-1} approx. 0 GO TO step 5
            !
            if (abs(first_diff)<threshold .and. abs(xn - xnm1)<threshold) then
                goto 5
            else
                !
                ! 4) ELSE (f'(x_n) /= 0) THEN
                !        Compute f''(x_n)
                !        Compute next point;
                !        x_{n+1} = x_n - f'(x_n)/f''(x_n) & f(x_{n+1}) < f(x_n)
                !        GO TO step 2
                !
                call finite_diff(method, 2, xn, h, second_diff)
                !
                xnp1 = xn - first_diff/second_diff
                !
                if (f(xnp1) < f(xn)) then
                    !
                    xnm1 = xn
                    xn = xnp1
                    !
                    goto 2
                    !
                end if
                !
            end if
            !
            ! 5) END program
            !
            5 continue
            !
            xmin = xn
            !
            return
        end subroutine Newton_Raphson
end module
