! =========================================================================================
!                           Newton-Raphson Method for Optimization
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

real(kind=8) function func(X, d)
! -----------------------------------------------------------------------------------------
! This function should be defined by the user of the program
! -----------------------------------------------------------------------------------------
!
! Inputs:
! - X: an array of real numbers
! - d: the number of variables in the array
!
! Outputs:
! - funcion: a real number
!
! -----------------------------------------------------------------------------------------
    integer :: d
    real(kind=8), dimension(d) :: X
    func = dsin(X(1) + X(2)) + (X(1) - X(2))**2 - 1.5*X(1) + 3.5*X(2) + 3
    return
end function

program NewtonRaphson

! -----------------------------------------------------------------------------------------
! This program computes the minimum of a given function using the Newton-Raphson method
! -----------------------------------------------------------------------------------------
!
! Inputs:
! - funcion:   A function defined at the beginning of this program. It is imported as an
!              external function.
!
! Outputs:
! - on screen: The iteration, coordinates, function, convergence (error), Hessian and
!              Gradient
! - as a file: A file 'output.csv' is produced with the coordinates, the sum of the
!              Gradient and the convergence (error)
!
! -----------------------------------------------------------------------------------------

    ! Import the integration modules ------------------------------------------------------
    use dmethods, only : Hessian, Gradient, Invert, ShowHG

    implicit none

    ! Import the function defined at the beginning of the file ----------------------------
    external func

    ! Variables ---------------------------------------------------------------------------
    ! Function, derivative delta, sum of gradient, new value, old value, error
    real(kind=8) :: func
    real(kind=8) :: delta, criterion, funval, funvalO, abserr
    real(kind=8), allocatable :: coordsI(:), coordsO(:), G(:)
    real(kind=8), allocatable :: H(:,:), iH(:,:)
    integer :: i, j, d, iter, max_iter

    ! Providing some GUI to the user ------------------------------------------------------
    write(*, *) '--------------------------------------------------------------'
    write(*, *) '|                 Optimization of Functions                  |'
    write(*, *) '--------------------------------------------------------------'
    write(*, *) '|                                                            |'
    write(*, *) '|  This program will attempt to find a minima of the         |'
    write(*, *) '|  following function:                                       |'
    write(*, *) "|                                                            |"
    write(*, *) '|                                2                           |'
    write(*, *) "|  f(x, y) = sin(x + y) + (x - y)  - 1.5 . x + 3.5 . y + 3   |"            
    write(*, *) '|                                                            |'
    write(*, *) '|  Using the Newton-Raphson method, this program will try    |'
    write(*, *) "|  to find one of this function's minima. To do so, it will  |"
    write(*, *) '|  compute both the gradient, and the Hessian of the         |'
    write(*, *) '|  function. After inverting the Hessian, and multiplying    |'
    write(*, *) '|  it with the gradient, it will obtain the direction as to  |'
    write(*, *) '|  where to move to minimize the value of the gradient.      |'
    write(*, *) '|                                                            |'
    write(*, *) '|                         Computing ...                      |'
    write(*, *) '|                                                            |'
    write(*, *) '--------------------------------------------------------------'

    ! Initializing variables --------------------------------------------------------------
    ! --> Number of dimensions that the function will use
    d = 2
    ! --> Allocate space for the vectors with the coordinates
    allocate(coordsI(d))
    allocate(coordsO(d))
    ! --> Allocate space for the value of the gradient
    allocate(G(d))
    ! --> Allocate space for the value of the hessian and its inverse
    allocate(H(d,d))
    allocate(iH(d,d))
    ! --> Initializing coordinates arrays (initial coords: 1, 3)
    coordsI = 1
    coordsO = 0
    coordsI(2) = 3
    ! --> Sum of the values of the gradient (if this were 0 -> minimum found!)
    criterion = 1
    ! --> Counter of the iterations
    iter = 0
    ! --> Maximum number of iterations before forcibly stopping
    max_iter = 50
    ! --> Old value of the function in the preivous coordinates
    funvalO = 0
    ! --> New value of the function in the actual coordinates
    funval = func(coordsI, d)
    ! --> Absolute error between the old value and the new value
    abserr = abs(funval - funvalO)
    ! --> Finite interval to compute the numeric derivatives
    delta = 1D-6

    open(44, file='output.csv', status='replace')
    write(44, "(A)") "           X           Y           Z      Gradient_Value Absolute_Difference"
    write(44, "(F12.8, F12.8, F12.8, E20.10, E20.10)") coordsI(1), coordsI(2), funval, criterion, abserr

    ! Check if the the iterations exceed the maximum value, or if the method
    ! has converged through 2 criteria:
    ! (a) The difference between the old and new values is less than 10^-8
    ! (b) The gradient is less than 10^-8
    do while ((abserr > 1D-8) .and. (criterion > 1D-8) .and. (iter < max_iter))

        ! Providing additional GUI to the user --------------------------------------------
        write(*, *) ""
        write(*, *) '--------------------------------------------------------------'
        write(*, *) '|   Iter         X         Y    f(x,y)               Error   |'
        write(*, *) '--------------------------------------------------------------'
        write(*, "(' ', A, I5, F10.6, F10.6, F10.6)", advance='no') "|  ", iter, coordsI(1), coordsI(2), func(coordsI, 2)
        write(*, "(E20.8, A)") abserr, "   |"

        funvalO = funval                            ! The new value of the function
                                                    ! becomes the old value
        call Hessian(func, d, coordsI, delta, H)    ! Compute Hessian in the new coords
        call Invert(H, d, iH)                       ! Invert the Hessian matrix
        call Gradient(func, d, coordsI, delta, G)   ! Compute Gradient in the new coords

        call ShowHG(H, G)                           ! Show both the Hessian and Gradient

        coordsO = coordsI - matmul(iH, G)           ! Calculate new coordinates
        
        criterion = sum(G)                          ! Total slope by sum of Gradient
        coordsI = coordsO                           ! The new coords become the old ones
        funval = func(coordsI, d)                   ! Value of function in new point
        abserr = abs(funval - funvalO)              ! Difference between old and new values
        write(44, "(F12.8, F12.8, F12.8, E20.10, E20.10)") coordsO(1), coordsO(2), funval, criterion, abserr
        iter = iter + 1                             ! Increaste the number of iterations

    end do

    close(44)

    ! Providing final GUI to the user -----------------------------------------------------
    write(*, *) ""
    write(*, *) '--------------------------------------------------------------'
    write(*, *) '|                        Final Results                       |'
    write(*, *) '--------------------------------------------------------------'
    write(*, *) '|   Iter         X         Y    f(x,y)               Error   |'
    write(*, *) '--------------------------------------------------------------'
    write(*, "(' ', A, I5, F10.6, F10.6, F10.6)", advance='no') "|  ", iter, coordsI(1), coordsI(2), func(coordsI, 2)
    write(*, "(E20.8, A)") abserr, "   |"

    call Hessian(func, d, coordsI, delta, H)        ! Compute Hessian in the last coords
    call Gradient(func, d, coordsI, delta, G)       ! Compute Gradient in the last coords

    call ShowHG(H, G)                               ! Show both the Hessian and Gradient

end program NewtonRaphson