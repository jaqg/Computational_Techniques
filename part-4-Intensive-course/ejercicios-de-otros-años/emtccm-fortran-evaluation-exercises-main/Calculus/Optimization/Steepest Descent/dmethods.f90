! =========================================================================================
!               Steepest Descent Method for Optimization (Calculus Subroutines)
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

module dmethods

    implicit none

    ! Establishing the subroutines that will be provided to the external program
    public :: Gradient, ShowGrad

    contains

    subroutine Partial1(f, d, entry, coordsI, delta, P1)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to calculate the partial derivative of a function, using a
    ! finite centered method with respect to a single coordinate
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f:       the function
    ! - d:       the number of coordinates it uses
    ! - entry:   the coordinate to be used to derivate
    ! - coordsI: a vector with the coordinates to be used to evaluate the derivative
    ! - delta:   the dinite difference to be used to derivate numerically
    ! Outputs
    ! - P1:      a real number with the value of the derivative on the given point with
    !            respect to the given coordinate
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Importing the function ----------------------------------------------------------
        external f

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: d, entry
        real(kind=8), intent(in) :: delta
        real(kind=8), allocatable, intent(in) :: coordsI(:)
        real(kind=8), intent(out) :: P1

        ! Internal variables --------------------------------------------------------------
        real(kind=8) :: f
        real(kind=8), allocatable :: active(:)

        ! Assigning memory to a dummy vector which will be used to select the coordinate
        allocate(active(d))
        ! Initialize the vector
        active = 0
        ! Activate the coordinate to be used to derivate
        active(entry) = delta

        ! Compute the derivative
        P1 = (f(coordsI + active, d) - f(coordsI - active, d)) / (2 * delta)

    end subroutine Partial1

    subroutine Gradient(f, d, coordsI, delta, G)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to calculate the Gradient of a function, using partial derivatives
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f:       the function
    ! - d:       the number of coordinates it uses
    ! - coordsI: a vector with the coordinates to be used to evaluate the derivative
    ! - delta:   the dinite difference to be used to derivate numerically
    ! Outputs
    ! - G:       a vector with the partial derivative of the function over each coordinate
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Importing the function ----------------------------------------------------------
        external f

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: d
        real(kind=8), intent(in) :: delta
        real(kind=8), allocatable, intent(in) :: coordsI(:)
        real(kind=8), allocatable, intent(out) :: G(:)

        ! Internal variables --------------------------------------------------------------
        real(kind=8) :: f
        real(kind=8) :: temp
        integer :: i

        ! Assigning memory to the vector which will store the Gradient
        allocate(G(d))
        ! Initialize the vector
        G = 0
        ! Initialize the variable which will temporarily hold the partial derivative
        temp = 0

        do i = 1, d                                      ! Cycle through every coordinate
            call Partial1(f, d, i, coordsI, delta, temp) ! Partial derivative with respect
                                                         ! to i-th coordinate
            G(i) = temp                                  ! Store partial derivative
        end do

    end subroutine Gradient

    subroutine ShowGrad(grad)
    ! -------------------------------------------------------------------------------------
    ! Subroutine to show the Gradient on screen
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - grad:    a vector containing the gradient
    ! Outputs
    ! - On screen: the provided gradient
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        real(kind=8), allocatable, intent(in) :: grad(:)

        ! Internal variables --------------------------------------------------------------
        integer :: d, i

        ! Find out the size of the gradient
        d = size(grad)

        write(*, *) '--------------------------------------------------------------'
        write(*, "(' ', A)") "|                  Gradient over new coords                  |"
        write(*, *) '--------------------------------------------------------------'
        write(*, *) '|                                                            |'

        do i = 1, d
            write(*, "(' ', A, E16.8, A)") "|                 ", grad(i), "                           |"
        end do

        write(*, *) '|                                                            |'
        write(*, *) '--------------------------------------------------------------'

    end subroutine ShowGrad

end module dmethods