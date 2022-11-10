! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 18:19:06 25-10-2022 |
! +--------------------------------------------+

program final_project
    !
    ! Final project:
    ! Time evolution simulation of a population of rabbits and foxes with the
    ! Simple and Logistic Lotka-Volterra model, and different integration
    ! methods:
    !   - Taylor's method
    !   - Euler's method
    !   - Modified Euler's method
    !   - Runge-Kutta's 4th order method
    !
    ! Modules
    !
    use IO
    !
    ! Variable definition
    !
    implicit none
    !
    integer(kind=8) :: i, j, n, final_n
    !
    procedure(), pointer :: method => null()
    real(kind=8), external :: LV
    external :: Euler, modEuler, RK4
    !
    !
    ! ========================= START OF THE PROGRAM =========================
    !
    !
    ! --- Read input ---
    !
    call read_input
    !
    ! Number of (time) steps
    !
    call time_steps(t0, tf, h, n)
    !
    ! Allocate arrays
    !
    call allocate_arrays(n)
    !
    ! Set the pointer 'method' to the chosen method
    !
    if (themethod == "Euler" .or. themethod == "E") then
        method => Euler
    elseif (themethod == "ModEuler" .or. themethod == "ME") then
        method => modEuler
    ! elseif (themethod == "Taylor" .or. themethod == "T") then
    !     method => Taylor
    elseif (themethod == "Runge-Kutta" .or. themethod == "RK4") then
        method => RK4
    else
        write(*,*) 'main.f90 ERROR: wrong method input'
    end if
    !
    ! Assign needed parameters/variables
    !
    call assign_params
    !
    ! Main loop
    !
    final_n = 0
    lt1: do i = 1, n
        !
        ! If any individual becomes <0, stop the calculation
        !
        do j = 1, nsp
            if ( y(i,j) < 0 ) exit
        end do
        !
        ! Assign a variable 'final_n' as the number of iteration
        !
        final_n = i
        !
        ! Calculate the time
        !
        t(i) = t0 + (dble(i-1) * h)
        !
        ! Call the method as:
        !
        ! method( predator_t, prey_t, predator_{t+h}, prey_{t+h} )
        !
        call method( y(i,2), y(i,1), y(i+1,2), y(i+1,1) )
        call Rk4( yRK(i,2), yRK(i,1), yRK(i+1,2), yRK(i+1,1) )
        !
    end do lt1
    !
    ! Write results
    !
    call write_output(n,final_n)
    !
    write(*,*) 'Program compiled successfully.'
    stop
endprogram final_project
