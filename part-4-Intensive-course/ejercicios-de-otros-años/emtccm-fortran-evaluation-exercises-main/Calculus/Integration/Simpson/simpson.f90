! =========================================================================================
!                             Simpson Method for Integration
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

real(kind=8) function funcion(x)
! -----------------------------------------------------------------------------------------
! This function should be defined by the user of the program
! -----------------------------------------------------------------------------------------
!
! Inputs:
! - x: a real number
!
! Outputs:
! - funcion: a real number
!
! -----------------------------------------------------------------------------------------
    real(kind=8) :: x
    funcion = dsin(x**2) - dcos(2*x)
    return
end function

program Integral

! -----------------------------------------------------------------------------------------
! This program computes the integral of a given function using the Simpson method
! -----------------------------------------------------------------------------------------
!
! Inputs:
! - funcion: a function defined at the beginning of this program. It is imported as an
!            external function.
!
! Outputs:
! - on screen: it shows the value of integrating the function in a given interval.
!
! -----------------------------------------------------------------------------------------

    ! Import the integration modules ------------------------------------------------------
    use imethods, only : int_simp

    implicit none

    ! Import the function defined at the beginning of the file ----------------------------
    external funcion

    ! Variables ---------------------------------------------------------------------------
    real(kind=8) :: funcion, a, b, E
    real(kind=8) :: Is, oIs
    integer :: ns, iter

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
    write(*, *) '| Using the Simpson method, starting with 2 intervals. |'
    write(*, *) '|                                                      |'
    write(*, *) '|                         Computing ...                |'
    write(*, *) '|                                                      |'
    write(*, *) '--------------------------------------------------------'

    ! Initializing variables --------------------------------------------------------------
    ! --> Integration Limits
    a = 1.0
    b = 3.0
    ! --> Number of Intervals (They will be multiplied by 2 before the Simpson method)
    ns = 1
    ! --> Number of Iterations
    iter = 0
    ! --> New and old values of the Integral
    Is = 1
    oIs = 0
    ! --> Error (Convergence criterion)
    E = 1
    
    write(*, *) "|  Iteration Intervals  Integral Value  Absolute Error |"
    write(*, *) '--------------------------------------------------------'
    
    ! Computing the integral by passing the aforementioned values -------------------------
    do while (E > 1D-8)                       ! Convergence threshold
        oIs = Is                              ! Making old value equal to the new one
        iter = iter + 1                       ! Incrementing the iteration step
        ns = ns * 2                           ! Duplicating the number of intervals
        call int_simp(funcion, a, b, ns, Is)  ! Calling the Simpson subroutine
        E = abs(Is - oIs)                     ! Computing the error
        write(*, "(' ', A, I10, I10, F16.10, E16.6, A)") "| ", iter, ns, Is, E, " |"
    end do

    ! Displaying final results ------------------------------------------------------------
    write(*, *) '--------------------------------------------------------'
    write(*, *) "|                    Final Results                     |"
    write(*, *) '--------------------------------------------------------'
    write(*, *) '|                                                      |'
    write(*, "(' ', A, I10, A)") "|  Iteration step:                 ", iter, "          |"
    write(*, "(' ', A, I10, A)") "|  Interval number:                ", ns, "          |"
    write(*, "(' ', A, I10, A)") "|  Number of points employed:      ", ns + 1, "          |"
    write(*, "(' ', A, F16.10, A)") "|  Quadrature value:         ", Is, "          |"
    write(*, *) '|                                                      |'
    write(*, *) '--------------------------------------------------------'

end program Integral