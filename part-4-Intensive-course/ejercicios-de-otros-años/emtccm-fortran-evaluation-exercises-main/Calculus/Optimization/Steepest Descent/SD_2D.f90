! =========================================================================================
!                        Steepest Descent Method for Optimization
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

program SteepestDescent

! -----------------------------------------------------------------------------------------
! This program computes the minimum of a given function using the Steepest Descent method
! -----------------------------------------------------------------------------------------
!
! Inputs:
! - funcion:   A function defined at the beginning of this program. It is imported as an
!              external function.
!
! Outputs:
! - on screen: The iteration, coordinates, function, convergence (error), and Gradient
! - as a file: A file 'output.csv' is produced with the coordinates, the sum of the
!              Gradient and the convergence (error)
!
! -----------------------------------------------------------------------------------------

    ! Import the integration modules ------------------------------------------------------
    use dmethods, only : Gradient, ShowGrad

    implicit none

    ! Import the function defined at the beginning of the file ----------------------------
    external func

    ! Variables ---------------------------------------------------------------------------
    ! Function, SD step gamma, derivative delta, sum of gradient, new val, old val, error
    real(kind=8) :: func
    real(kind=8) :: gamma, delta, criterion, funval, funvalO, abserr
    real(kind=8), allocatable :: coordsI(:), coordsO(:), G(:)
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
    write(*, *) '|  Using the Steepest Descent method, this program will try  |'
    write(*, *) "|  to find one of this function's minima. To do so, it will  |"
    write(*, *) '|  compute the gradient, of the function. By multiplying the |'
    write(*, *) '|  gradient with gamma, it will obtain the direction as to   |'
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
    ! --> Initializing coordinates arrays (initial coords: 1, 3)
    coordsI = 1
    coordsO = 0
    coordsI(2) = 3
    ! --> Sum of the values of the gradient (if this were 0 -> minimum found!)
    criterion = 1
    ! --> Counter of the iterations
    iter = 0
    ! --> Maximum number of iterations before forcibly stopping
    max_iter = 40
    ! --> Old value of the function in the preivous coordinates
    funvalO = 0
    ! --> New value of the function in the actual coordinates
    funval = func(coordsI, d)
    ! --> Absolute error between the old value and the new value
    abserr = abs(funval - funvalO)
    ! --> Value of gamma to be multiplied with the gradient to move to optimized position
    gamma = 0.3
    ! --> Finite interval to compute the numeric derivatives
    delta = 1D-6

    open(44, file='output.csv', status='replace')
    write(44, "(A)") "           X           Y           Z      Gradient_Value Absolute_Difference"
    write(44, "(F12.8, F12.8, F12.8, E20.10, E20.10)") coordsI(1), coordsI(2), func(coordsI, 2), criterion, abserr

    ! Check if the the iterations exceed the maximum value, or if the method
    ! has converged through 2 criteria:
    ! (a) The difference between the old and new values is less than 10^-8
    ! (b) The gradient is less than 10^-8
    do while ((abserr > 1D-9) .and. (criterion > 1D-9) .and. (iter < max_iter))

    	! Providing additional GUI to the user --------------------------------------------
        write(*, *) ""
        write(*, *) '--------------------------------------------------------------'
        write(*, *) '|   Iter         X         Y    f(x,y)               Error   |'
        write(*, *) '--------------------------------------------------------------'
        write(*, "(' ', A, I5, F10.6, F10.6, F10.6)", advance='no') "|  ", iter, coordsI(1), coordsI(2), func(coordsI, 2)
        write(*, "(E20.8, A)") abserr, "   |"

        funvalO = funval                              ! The new value of the function
                                                      ! becomes the old value
        call Gradient(func, d, coordsI, delta, G)     ! Compute Gradient in the new coords

        G = G * 1/sqrt(dot_product(G, G))             ! Normalizing the Gradient

        call ShowGrad(G)                              ! Show the Gradient

        coordsO = coordsI - gamma * G                 ! Calculate new coordinates
        
        criterion = sum(G)                            ! Total slope by sum of Gradient
        coordsI = coordsO                             ! The new coords become the old ones
        funval = func(coordsI, d)                     ! Value of function in new point
        abserr = abs(funval - funvalO)                ! Difference between old and new vals
        write(44, "(F12.8, F12.8, F12.8, E20.10, E20.10)") coordsO(1), coordsO(2), func(coordsO, 2), criterion, abserr
        iter = iter + 1                               ! Increaste the number of iterations

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

    call Gradient(func, d, coordsI, delta, G)         ! Compute Gradient in the last coords

    call ShowGrad(G)                                  ! Show the Gradient

end program SteepestDescent