! =====================================================================================
!                       Gauss-Legendre Method for Integration
! -------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =====================================================================================

real(kind=8) function func(x)
! -----------------------------------------------------------------------------------------
! This function should be defined by the user of the program
! -----------------------------------------------------------------------------------------
!
! Inputs:
! - x: a real number
!
! Outputs:
! - func: a real number
!
! -----------------------------------------------------------------------------------------
    real(kind=8) :: x
    func = dsin(x**2) - dcos(2*x)
    return
end function

program GaussLeg
! -----------------------------------------------------------------------------------------
! This program computes the integral of a given function using the Gauss Legendre method
! -----------------------------------------------------------------------------------------
!
! Inputs:
! - func: a function defined at the beginning of this program. It is imported as an
!            external function.
!
! Outputs:
! - on screen: it shows the value of integrating the function in a given interval.
!
! -----------------------------------------------------------------------------------------

    ! Import the integration module -------------------------------------------------------
    use SubGauleg, only : sub_GauLeg

    implicit none

    ! Import the function defined at the beginning of the file ----------------------------
    external func

    ! Variables ---------------------------------------------------------------------------
    ! Function, integration limits, area, error variables and old iteration value
    real(kind=8) :: func, eval, oeval, a, b, E
    real(kind=8), allocatable :: w(:), t(:)
    integer :: i, j, k, l

    ! Providing some GUI to the user ------------------------------------------------------
    write(*, *) '--------------------------------------------------------'
    write(*, *) '|              Integration of Functions                |'
    write(*, *) '--------------------------------------------------------'
    write(*, *) '|                                                      |'
    write(*, *) '| This program will evaluate the following:            |'
    write(*, *) '|                                                      |'
    write(*, *) "|              3                                       |"            
    write(*, *) "|              .-                                      |"
    write(*, *) "|              |      / 2\                             |"
    write(*, *) "|             -'  sin \x / - cos(2 . x) dx             |"
    write(*, *) "|              1                                       |"
    write(*, *) '|                                                      |'
    write(*, *) '| Using the Gauss-Legendre method, from 2 to 10 points |'
    write(*, *) '|                                                      |'
    write(*, *) '|                         Computing ...                |'
    write(*, *) '|                                                      |'
    write(*, *) '--------------------------------------------------------'

    ! Initializing variables --------------------------------------------------------------
    ! --> Integration Limits (*Note: Gauss-Legendre works best with limits -1 and 1)
    a = -1
    b = 1
    ! --> Initializing k
    k = 1
    ! --> Error (Convergence criterion)
    E = 1
    ! --> New value of the integral
    eval = 0
    ! --> Old value of the integral
    oeval = 0

    write(*, *) "|        Points  Integral Value  Absolute Error        |"
    write(*, *) '--------------------------------------------------------'
    do k=2, 10                                 ! Starting with k = 2 points
        allocate(w(k))                         ! Allocate memory for w (the weights) and
        allocate(t(k))                         ! for t (x values - Legendre polynom. roots)
        w = 0                                  ! Initialize both arrays
        t = 0
        call sub_GauLeg(a, b, t, w, k)         ! Compute the values of t and w
        oeval = eval                           ! The new value becomes the old value
        eval = 0                               ! The new value is reset
        do l=1, k                              ! The sum of the (new) weighted function is
            eval = eval + w(l) * func(t(l)+2)  ! computed. (*Note: the "+2" displaces the
        end do                                 ! function by 2-, to match the limits.)
        E = abs(eval - oeval)                  ! The (error) difference is calculated
        write(*, "(' ', A, I6, F16.8, E16.8, A)") "|        ", k, eval, E, "        |"
        if (E < 1D-8) then                     ! If the integral value has converged, ...
            exit                               ! ... exit the loop
        end if
        deallocate(t)                          ! Deallocate t and w ...
        deallocate(w)
    end do

    write(*, *) '--------------------------------------------------------'
    write(*, *) "|                    Final Results                     |"
    write(*, *) '--------------------------------------------------------'
    write(*, *) "|        Quadrature Value     Quadrature Points        |"
    write(*, *) '--------------------------------------------------------'
    write(*, "(' ', A, F16.12, I22, A)") "|        ", eval, k-1, "        |"
    write(*, *) '--------------------------------------------------------'

end program GaussLeg