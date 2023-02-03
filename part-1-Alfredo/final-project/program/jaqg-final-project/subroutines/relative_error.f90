! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 00:41:19 15-11-2022 |
! +--------------------------------------------+
subroutine relerror(x, y, theerr)
    !
    ! Subroutine to compute the error of x(:,:) and y(:,:)
    !
    implicit none
    !
    real(kind=8), dimension(:,:), allocatable, intent(in) :: x, y
    real(kind=8), dimension(:,:), allocatable, intent(out) :: theerr
    !
    ! Dummy variables
    !
    integer :: i, j, ierr
    !
    ! Check that sizes match
    !
    if (size(x,1) /= size(y,1)) then
        write(*,*) 'error.f90: size(x,1) /= size(y,1)'
        stop
    elseif (size(x,2) /= size(y,2)) then
        write(*,*) 'error.f90: size(x,2) /= size(y,2)'
        stop
    end if
    !
    ! Allocate error array
    !
    allocate(theerr(size(x,1),size(x,2)), stat=ierr)
    if (ierr .ne. 0) stop 'error.f90: Error in allocation of err'
    !
    do i = 1, size(x,2)
        do j = 1, size(x,1)
            theerr(j,i) = abs( ( y(j,i) - x(j,i) ) / y(j,i) )
        end do
    end do
    !
    return
end subroutine relerror
