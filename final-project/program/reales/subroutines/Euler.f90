! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 23:16:48 05-11-2022 |
! +---------------------------------------------+
subroutine Euler(f, y0, t0, tf, h, t, y)
    !
    ! Euler's method:
    ! y_{n+1} = y_n + h * f(t_n, y_n)
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
    integer :: i, j, ierr, nt
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
    if (ierr .ne. 0) stop 'Euler.f90: Error in allocation of t'
    !
    allocate(y(nt,size(y0)), stat=ierr)
    if (ierr .ne. 0) stop 'Euler.f90: Error in allocation of y'
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
        !!
        !do j = 1, size(y0)
        !    if ( y(i,j) < 0 ) exit
        !end do
        !
        ! Time
        !
        t(i+1) = t0 + (dble(i) * h)
        !
        ! y(t)
        !
        y(i+1,:) = y(i,:) + h * f( t(i), y(i,:) )
        !
    end do
    !
    return
end subroutine Euler
