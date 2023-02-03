! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 12:01:03 26-10-2022 |
! +----------------------------------------------+

function LV(t, y) result(res)
    !
    ! Lotka-Volterra model.
    !
    ! In general, we can write the L-V equations as:
    !
    ! dy/dt = f(t,y) = A * y + B * y**2 + C * x * y
    !
    ! where A, B, C are parameters (constants).
    !
    ! Then, we have two models:
    !
    ! - Simple (B = 0):
    !  dy/dt = A * y + C * x * y
    !
    ! - Logistic (B /= 0):
    !  dy/dt = A * y + B * y**2 + C * x * y
    !
    use IO, only: params
    !
    implicit none
    !
    ! Input time t is passed as a scalar
    !
    ! Species is treated as y(i,:) for a -time- step 'i', so it can be passed
    ! as a vector y(:)
    !
    ! The calculated species are output as a vector of same dimensions as
    ! input species
    !
    real(kind=8), optional, intent(in) :: t
    real(kind=8), dimension(:), intent(in) :: y
    real(kind=8), dimension(:), allocatable :: res
    !
    ! Dummy variables
    !
    integer :: i, ierr
    !
    ! Dumb use of t to remove warning: "unused variable"
    !
    if(.false.) write(*,*) t
    !
    ! Allocate 'res'
    !
    allocate(res(size(y)), stat=ierr)
    if (ierr .ne. 0) stop 'LV.f90: Error in allocation of res'
    !
    ! Calculation
    !
    do i = 1, size(y)-1
        !
        ! As (in this case) species are store in array 'y' as pairs (prey/pred)
        ! we can ran the calculation also by pairs as
        !
        ! Prey -> y(i) -> res(i)
        !
        res(i) = params(1,i) * y(i) + &
               & params(2,i) * y(i)**2 + &
               & params(3,i) * y(i) * y(i+1)
        !
        ! Predator -> y(i+1) -> res(i+1)
        !
        res(i+1) = params(1,i+1) * y(i+1) + &
                 & params(2,i+1) * y(i+1)**2 + &
                 & params(3,i+1) * y(i+1) * y(i)
        !
    end do
    !
    return
end function LV
