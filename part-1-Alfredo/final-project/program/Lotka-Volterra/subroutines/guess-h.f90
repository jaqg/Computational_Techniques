! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Monday 11:48:23 21-11-2022 |
! +-------------------------------------------+
subroutine guess_h(x0, n, lim, alpha, alphaprime, h)
    !
    ! Subroutine to compute a guess for time step 'h'
    !
    ! I consider h as the time step needed in order to have 'n' time steps
    ! in the range [t0, t], where t is the time a poblation takes to decrease
    ! from y_0 until y_0/lim
    !
    ! so
    !
    ! t = 1/alpha * ln( (alpha*lim - alphapime*x0)/(alpha - alphapime*x0) )
    !
    ! and
    !
    ! h = t/n
    !
    ! Notice that when alphaprime = 0, it is the solution for the simple model
    !
    implicit none
    integer, intent(in) :: n
    real(kind=8), intent(in) :: x0, lim, alpha, alphaprime
    real(kind=8), intent(out) :: h
    !
    ! Dummy variables
    !
    real(kind=8) :: t
    !
    t = 1.0_8/alpha * &
      & log( (alpha * lim - alphaprime * x0)/(alpha - alphaprime * x0) )
    !
    h = t/dble(n)
    !
    return
end subroutine guess_h
