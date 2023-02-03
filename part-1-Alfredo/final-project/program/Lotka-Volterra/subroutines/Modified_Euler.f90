! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 23:45:16 05-11-2022 |
! +---------------------------------------------+
subroutine modEuler(f, y0, t0, tf, h, threshold, t, y)
    !
    ! Modified Euler's method with the trapezoid method:
    ! y_{k+1}^{(n+1)} = y_k + h/2 * ( f(t_k, y_k) + f(t_{k+1}, y_{k+1}^{(n)}) )
    !
    implicit none
    !
    ! Initial values are passed as a 1D array 'y0'
    ! Initial 't0' and final time 'tf', as well as time step 'h', are passed
    ! as scalars
    ! Results are stored in 1D array for time 't', and 2D array for 'y'
    !
    real(kind=8), dimension(:), intent(in) :: y0
    real(kind=8), intent(in) :: t0, tf, h, threshold
    real(kind=8), dimension(:), allocatable, intent(out) :: t
    real(kind=8), dimension(:,:), allocatable, intent(out) :: y
    !
    ! Dummy variables
    !
    integer :: i, j, ierr, nt
    real(kind=8), dimension(:), allocatable :: ykn, yknp1
    !
    ! Interface
    !
    abstract interface
        function func(t, y) result(res)
            implicit none
            real(kind=8), optional, intent(in) :: t
            real(kind=8), dimension(:), intent(in) :: y
            real(kind=8), dimension(:), allocatable :: res
        end function func
    end interface
    !
    procedure(func) :: f
    !
    ! Number of points
    !
    nt = int( (tf - t0)/h ) + 1
    !
    ! Allocate arrays
    !
    allocate(t(nt), stat=ierr)
    if (ierr .ne. 0) stop 'modEuler.f90: Error in allocation of t'
    !
    allocate(y(nt,size(y0)), stat=ierr)
    if (ierr .ne. 0) stop 'modEuler.f90: Error in allocation of y'
    !
    allocate(ykn(size(y0)), stat=ierr)
    if (ierr .ne. 0) stop 'modEuler.f90: Error in allocation of ykn'
    !
    allocate(yknp1(size(y0)), stat=ierr)
    if (ierr .ne. 0) stop 'modEuler.f90: Error in allocation of yknp1'
    !
    ! Initial values
    !
    y(1,:) = y0(:)
    t(1) = t0
    !
    ! Calculation
    !
    ml: do i = 1, nt-1
        !
        ! If any individual becomes <0, stop the calculation
        !
        ! TODO checkear si hay alguna funcion del tipo any()
        do j = 1, size(y0)
            if ( y(i,j) < 0 ) exit
        end do
        !
        ! Time
        !
        t(i+1) = t0 + (dble(i) * h)
        !
        ! Initialitation
        !
        ykn(:) = y(i,:) + h * f(t(i), y(i,:)) ! -> y_k^{(0)}
        !
        cl: do
            !
            ! Calculation of y_{k+1}^{(n+1)}
            ! ( Note: on first iteration y_{k+1}^{(n+1)} -> y_{k+1}^{(1)} )
            !
            yknp1(:) = y(i,:) + h/2.0_8*(f(t(i), y(i,:)) + f(t(i+1), ykn(:)))
            !
            ! Check for convergence
            !
            if ( all( abs(yknp1 - ykn) < threshold ) ) exit cl
            !
            ykn(:) = yknp1(:)
            !
        end do cl
        !
        ! Store the final -converged- result
        !
        y(i+1,:) = yknp1(:)
        !
    end do ml
    !
    return
end subroutine modEuler
