module module1
    implicit none
    character(len=34), parameter :: form10 = "('------------------------------')"
    contains
        subroutine cuadrado(X, Y)
            implicit none
            real(kind=8), dimension(:,:), intent(in) :: X
            real(kind=8), dimension(:,:), allocatable, intent(out) :: Y
            integer :: i, j, ierr
            integer :: rowX, colX
            !
            rowX = size(X,1)
            colX = size(X,2)
            !
            allocate(Y(rowX, colX), stat=ierr)
            if (ierr .ne. 0) stop 'Error in allocation of Y'
            !
            do i = 1, rowX
                do j = 1, colX
                    Y(i,j) = X(i,j)**2
                end do
            end do
            !
            write(*,form10)
            !
            return
        end subroutine cuadrado
end module
