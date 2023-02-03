! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 15:45:39 09-11-2022 |
! +----------------------------------------------+
subroutine RK4(f, y0, t0, tf, h, t, y)
    !
    ! Runge-Kutta's 4th order method:
    ! y_{k+1} = y_k + 1/6 * ( K_1 + 2K_2 + 2K_3 + K_4 )
    !
    !
    implicit none
    !
    ! Initial values are passed as a 1D array 'y0'
    ! Initial 't0' and final time 'tf', as well as time step 'h', are passed
    ! as scalar
    ! Results are stored in 1D array for time 't', and 2D array for 'y'
    !
    real(kind=8), dimension(:), intent(in) :: y0
    real(kind=8), intent(in) :: t0, tf, h
    real(kind=8), dimension(:), allocatable, intent(out) :: t
    real(kind=8), dimension(:,:), allocatable, intent(out) :: y
    !
    ! Dummy variables
    !
    integer :: i, ierr, nt
    real(kind=8), dimension(:), allocatable :: K1, K2, K3, K4
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
    if (ierr .ne. 0) stop 'RungeKutta.f90: Error in allocation of t'
    !
    allocate(y(nt,size(y0)), stat=ierr)
    if (ierr .ne. 0) stop 'RungeKutta.f90: Error in allocation of y'
    !
    allocate(K1(size(y0)), stat=ierr)
    if (ierr .ne. 0) stop 'RungeKutta.f90: Error in allocation of K1'
    !
    allocate(K2(size(y0)), stat=ierr)
    if (ierr .ne. 0) stop 'RungeKutta.f90: Error in allocation of K2'
    !
    allocate(K3(size(y0)), stat=ierr)
    if (ierr .ne. 0) stop 'RungeKutta.f90: Error in allocation of K3'
    !
    allocate(K4(size(y0)), stat=ierr)
    if (ierr .ne. 0) stop 'RungeKutta.f90: Error in allocation of K4'
    !
    ! Initial values
    !
    y(1,:) = y0(:)
    t(1) = t0
    !
    ! Calculation
    !
    do i = 1, nt-1
        !!
        !! If any individual becomes <0, stop the calculation
        !! TODO check if there's an 'any()' function
        !do j = 1, size(y0)
        !    if ( y(i,j) < 0 ) exit
        !end do
        !
        ! Time
        !
        t(i+1) = t0 + (dble(i) * h)
        !
        ! Constants K1, K2, K3, K4 (for y_{k+1})
        !
        ! K_1 = h * f(t_k, y_k)
        !
        K1(:) = h * f( t(i), y(i,:) )
        !
        ! K_2 = h * f(t_k + h/2, y_k + K_1/2)
        !
        K2(:) = h * f( t(i) + h/2.0_8, y(i,:) + K1/2.0_8 )
        !
        ! K_3 = h * f(t_k + h/2, y_k + K_2/2)
        !
        K3(:) = h * f( t(i) + h/2.0_8, y(i,:) + K2/2.0_8 )
        !
        ! K_4 = h * f(t_k + h, y_k + K_3)
        !
        K4(:) = h * f( t(i) + h, y(i,:) + K3 )
        !
        ! y(t)
        !
        ! y_{k+1} = y_k + 1/6 * ( K_1 + 2K_2 + 2K_3 + K_4 )
        y(i+1,:) = y(i,:) + 1.0_8/6.0_8 * ( K1 + 2.0_8*K2 + 2.0_8*K3 + K4 )
        !
    end do
    !
    return
end subroutine RK4
