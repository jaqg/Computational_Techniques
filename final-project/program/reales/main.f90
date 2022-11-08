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
    integer(kind=8) :: i
    integer(kind=8) :: n, final_n
    !
    procedure(), pointer :: method => null()
    real(kind=8), external :: LV
    external :: Euler, modEuler
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
    prey(1) = prey0
    predator(1) = predator0
    !
    ! Set the pointer 'method' to the chosen method
    !
    if (themethod == "Euler" .or. themethod == "E") then
        method => Euler
    elseif (themethod == "Modified Euler" .or. themethod == "ME") then
        method => modEuler
    ! elseif (themethod == "Taylor" .or. themethod == "T") then
    !     method => Taylor
    ! elseif (themethod == "Runge-Kutta" .or. themethod == "RK4") then
    !     method => RK4
    else
        write(*,*) 'main.f90 ERROR: wrong method input'
    end if
    !
    ! If the chosen model is the Simple one, then alphaprime and kappaprime
    ! must = 0 so the LV logistic model reduces to the simple one
    !
    if (themodel == "Simple" .or. themodel == "simple") then
        alphaprime = 0.0_8
        kappaprime = 0.0_8
    end if
    !
    ! Main loop
    !
    final_n = 0
    lt1: do i = 1, n
        !
        if (prey(i)<0 .or. predator(i)<0) exit
        !
        final_n = i
        !
        t(i) = t0 + (dble(i-1) * h)
        !
        call method( predator(i), prey(i), predator(i+1), prey(i+1) )
        ! call Euler( predator(i), prey(i), predator(i+1), prey(i+1) )
        ! call modEuler( predator(i), prey(i), predator(i+1), prey(i+1) )
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
