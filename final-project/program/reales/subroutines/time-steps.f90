! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 17:04:52 06-11-2022 |
! +-------------------------------------------+
subroutine time_steps(t0, tf, h, n)
    !
    ! Subroutine to calculate the number of (time) steps
    !
    implicit none
    !
    real(kind=8), intent(in) :: t0, tf, h
    integer(kind=8), intent(out) :: n
    !
    n = int( (tf - t0)/h )
    !
    return
end subroutine time_steps
