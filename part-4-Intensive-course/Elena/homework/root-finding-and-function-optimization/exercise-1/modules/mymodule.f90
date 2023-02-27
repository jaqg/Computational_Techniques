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
        ! $f(x,y) = \sin(x+y) + (x - y)^2 - 1.5x + 3.5y + 3$
        !
        f = dsin(x+y) + (x - y)**2 - 1.5_8 * x + 3.5_8 * y + 3.0_8
        !
        return
    end function f

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

    real(kind=8) function magn_vec(v)
        !
        ! Function to calculate the magnitude of a (real) vector
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: v
        integer :: i, n
        real(kind=8) :: suma
        !
        n = size(v,1)
        !
        suma = 0.0_8
        do i = 1, n
            suma = suma + v(i)**2
        end do
        !
        magn_vec = dsqrt(suma)
        !
        return
    end function magn_vec

    function norm_vec(v) result(v_norm)
        !
        ! Function that normalizes to one the vector v (real)
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: v
        real(kind=8), dimension(:), allocatable :: v_norm
        integer :: n, ierr
        !
        n = size(v,1)
        !
        allocate(v_norm(n), stat=ierr)
        if (ierr .ne. 0) stop &
            & 'mymodule.f90 norm_vec: Error in allocation of v_norm'
        !
        v_norm = v/magn_vec(v)
        !
        return
    end function norm_vec

    subroutine steepest_descent(initcoord, gamma, maxiter, threshold, &
        & fin_diff_method, h, coord, grad, normgrad, conv, mincoord, func, totiter)
        !
        ! Subroutine for the Steepest or gradient descent method
        !
        ! initcoord             real, double precision, 1D array
        ! gamma                 real, double precision
        ! maxiter               integer, single precision
        ! fin_diff_method       character
        ! h                     real, double precision
        ! coord                 real, double precision, 2D array
        ! grad                  real, double precision, 2D array
        ! normgrad              real, double precision, 2D array
        ! conv                  real, double precision, 2D array
        ! mincoord              real, double precision, 1D array
        ! func                  real, double precision, 1D array
        ! totiter               integer, single precision
        !
        ! initcoord: initial coordinates
        ! gamma: step size of the steepest descent method
        ! maxiter: maximum number of iterations
        ! fin_diff_method: finite difference method (forward,backward,central)
        ! h: h spacing for the finite difference method
        ! coord: coordinate matrix for each step (row) and dimension (col)
        ! grad: gradient matrix for each step (row) and dimension (col)
        ! normgrad: normalised gradient matrix
        ! conv: matrix with the convergence (error) values
        ! mincoord: 1D array with the coordinates of the minimum
        ! func: 1D array with the evaluation of the function, f(x,y)
        ! totiter: total number of iterations performed
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: initcoord
        real(kind=8), intent(in) :: gamma, h, threshold
        integer, intent(in) :: maxiter 
        character(len=*), intent(in) :: fin_diff_method 
        real(kind=8), dimension(:,:), allocatable, intent(out) :: coord
        real(kind=8), dimension(:,:), allocatable, intent(out) :: grad
        real(kind=8), dimension(:,:), allocatable, intent(out) :: normgrad
        real(kind=8), dimension(:,:), allocatable, intent(out) :: conv
        real(kind=8), dimension(:), allocatable, intent(out) :: mincoord
        real(kind=8), dimension(:), allocatable, intent(out) :: func
        integer, intent(out) :: totiter
        ! Dummy variables
        integer :: i, ierr 
        logical :: converged
        !
        ! Allocate needed arrays
        !
        allocate(coord(maxiter, size(initcoord, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'steepest_descent: Error in allocation of coord'
        !
        allocate(mincoord(size(initcoord, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop &
            & 'steepest_descent: Error in allocation of mincoord'
        !
        allocate(grad(maxiter, size(initcoord, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'steepest_descent: Error in allocation of grad'
        !
        allocate(normgrad(maxiter, size(initcoord, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop &
            &'steepest_descent: Error in allocation of normgrad'
        !
        allocate(func(maxiter), stat=ierr)
        if (ierr .ne. 0) stop 'steepest_descent: Error in allocation of func'
        !
        allocate(conv(maxiter, size(initcoord, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'steepest_descent: Error in allocation of conv'
        !
        ! 1. Choose an initial starting point P0
        !
        ! Store initial coordinates in the coord matrix
        !
        coord(1,:) = initcoord
        !
        ! Main loop
        !
        ml1: do i = 1, maxiter-1
            !
            ! 2. Calculation of the gradient
            !
            call finite_diff(fin_diff_method, 1, coord(i,1), coord(i,2), h, &
            & grad(i,1), grad(i,2))
            !
            ! Normmalise gradient vector
            !
            normgrad(i,:) = norm_vec(grad(i,:))
            !
            ! 3. Calculation of the steepest direction of f(P): 
            ! negative gradient of f at P
            !
            ! 4. Evaluate the next point P_{k+1}
            !
            ! P_{k+1} = P_k - \gamma_k grad( f(P_k) )
            !
            coord(i+1,:) = coord(i,:) - gamma * normgrad(i,:)
            !
            ! 5. Check for convergence: P_{k+1} - P_k < tolerance
            !
            conv(i,:) = dabs( coord(i+1,:) - coord(i,:) )
            converged = all( conv(i,:) < threshold, 1 )
            !
            if (converged .eqv. .True.) exit ml1
            !
            ! Store coordinates of minimum in a separate vector, total number
            ! of iterations and a 1D array with evaluation of function
            !
            mincoord(:) = coord(i,:)
            totiter = i
            func(i) = f( coord(i,1), coord(i,2) )
            !
        end do ml1
        !
        return
    end subroutine steepest_descent

end module
