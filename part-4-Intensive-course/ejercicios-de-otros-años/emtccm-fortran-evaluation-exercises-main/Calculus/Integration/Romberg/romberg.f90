! =========================================================================================
!                     Romberg Method for Integration of a Function
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

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
    func = dsin(X**2) - dcos(2*x)
    return
end function

program romberg
! -----------------------------------------------------------------------------------------
! This program computes the integral of a given function using the Romberg method
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
    use imethods, only : int_romberg

    implicit none

    ! Import the function defined at the beginning of the file ----------------------------
    external func

    ! Variables ---------------------------------------------------------------------------
    ! Function, integration limits, error variables and old error value
    real(kind=8) :: func, a, b, E, fE
    real(kind=8), allocatable :: R(:,:)
    integer :: i, j, k, s, ctrl
    character(len = 34) :: bar

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
    write(*, *) '| Using the Romberg method, a 10x10 triangular matrix  |'
    write(*, *) '| of integrals is built. The program may converge      |'
    write(*, *) '| before the 10th iteration. If it does, it will show  |'
    write(*, *) '| which iteration was the one where convergence was    |'
    write(*, *) '| achieved.                                            |'
    write(*, *) '|                         Computing ...                |'
    write(*, *) '|                                                      |'
    write(*, *) '--------------------------------------------------------'

    ! Initializing variables --------------------------------------------------------------
    ! --> Integration Limits
    a = 1
    b = 3
    ! --> Initializing k
    k = 10
    ! --> Value of k to be stored when convergence was achived
    s = 0
    ! --> Error (Convergence criterion)
    E = 1
    ! --> Error value to be stored when convergence was achived
    fE = 1
    ! --> Switch to be deactivated when convergence was achived
    ctrl = 1
    ! --> The long bar used when presenting the results of the matrix
    bar = "----------------------------------"

    ! Computing the integral by passing the aforementioned values -------------------------
    allocate(R(k,k))                            ! Allocating space for the new R matrix.
    R = 0                                       ! Initializing R.
    call int_romberg(func, a, b, k, R)          ! Running the Romberg method for that k

    ! Check if convergence was achieved ---------------------------------------------------
    do i = 2, 11                                ! Beginning with i = 2.
    	E = abs(R(i,i) - R(i,i-1))              ! Error of the last 2 Rs in that row
        if ((E < 1D-8) .and. (ctrl == 1)) then  ! If the error is below threshold and
                                                ! convergence hasn't been found before ...
            s = i                               ! Save the value of this iteration.
            fE = E                              ! Save the value of the error.
            ctrl = 0                            ! Deactivate the switch = value converged!
        end if
    end do
    
    ! Providing additional GUI to the user ------------------------------------------------
    write(*, *) "|           Romberg Triangular Matrix 10x10            |"
    write(*, *) bar//bar//bar//bar
    write(*, "(' ', A)", advance='no') "|           |"
    do i = 1, 10
        write(*, "(A, I2, A)", advance='no') "    R( k,", i, ")"
    end do
    write(*, *) " |"
    write(*, *) bar//bar//bar//bar

    ! Displaying the 10x10 matrix ---------------------------------------------------------
    do i=1, 10
        write(*, "(' ', A, I2, A)", advance='no') "| R(", i, ", j)  |"
        do j=1, i
            write(*, "(F12.8)", advance='no') R(i,j)
            if ((i == s) .and. (j == s)) then                       ! If convergence ...
                write(*,"(' ', A)", advance='no') "<-- Convergence!"! .. it converged here!
            end if
        end do
        write(*, *) ""
    end do
    write(*, *) bar//bar//bar//bar

    ! Displaying final results ------------------------------------------------------------
    write(*, *) "|                    Final Results                     |"
    write(*, *) '--------------------------------------------------------'
    if (ctrl == 0) then
        write(*, *) "|       Converged?    Integral Value      Position     |"
        write(*, *) '--------------------------------------------------------'
        write(*, "(' ', A, A10, F18.12, I7, I7, A)") "|       ", "True ", R(s,s), s, s, "     |"
    else
        write(*, *) "|       Converged?    Integral Value  Absolute Error   |"
        write(*, *) '--------------------------------------------------------'
        write(*, "(' ', A, A10, F18.12, E16.8, A)") "|       ", "False ", R(10,10), E, "   |"
    end if
    write(*, *) '--------------------------------------------------------'

end program romberg