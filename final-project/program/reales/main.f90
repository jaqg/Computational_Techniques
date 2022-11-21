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
    use Taylor_module
    !
    ! Variable definition
    !
    implicit none
    !
    integer(kind=8) :: i, j, n, final_n
    !
    procedure(func) :: LV
    procedure(methods) :: Euler, RK4
    procedure(methods2) :: modEuler
    procedure(error) :: abserror, relerror
    !
    ! ========================= START OF THE PROGRAM =========================
    !
    write(*,'(a)') '+-------------------------------------+'
    write(*,'(a)') '|        Program final_project        |'
    write(*,'(a)') '| Author: Jose Antonio Quinonero Gris |'
    write(*,'(a)') '+-------------------------------------+'
    write(*,*)
    !
    ! --- Read input ---
    !
    call read_input
    !
    write(*,'(2a)') 'The model is: ', themodel
    write(*,'(2a)') 'The method is: ', themethod
    !
    ! Number of (time) steps
    !
    call time_steps(t0, tf, h, n)
    !
    ! Allocate arrays
    !
    call allocate_arrays(n)
    !
    ! Assign needed parameters/variables
    !
    call assign_params
    !
    ! Guess time step
    !
    call guess_h(y0(2), 1000, 1.0D1, alpha, params(2,2), hguess)
    write(*,'(a, f10.4)') 'The recommended time step, h, is:', hguess
    write(*,*)
    !
    ! Main calculation
    !
    ! Calculate Runge-Kutta for comparison
    !
    call RK4( LV, y0, t0, tf, h, sol%t, sol%yRK )
    !
    if (themethod == "Euler" .or. themethod == "E") then
        !
        call Euler( LV, y0, t0, tf, h, sol%t, sol%y )
        !
    elseif (themethod == "ModEuler" .or. themethod == "ME") then
        !
        call modEuler( LV, y0, t0, tf, h, MEthreshold, sol%t, sol%y )
        !
    elseif (themethod == "Taylor" .or. themethod == "T") then
        !
        call Taylor(y0, t0, tf, h, TaylorTerms, sol%t, sol%y)
        !
    elseif (themethod == "Runge-Kutta" .or. themethod == "RK4") then
        !
        sol%y = sol%yRK
        !
    else
        write(*,*) 'main.f90 ERROR: wrong method input'
        !
    end if
    !
    ! Calculate the error
    !
    ! call abserror(sol%y, sol%yRK, sol%errorRK)
    call relerror(sol%y, sol%yRK, sol%errorRK)
    !
    ! If any individual becomes <0, stop
    !
    cl1: do i = 1, size(sol%y,1)
        !
        do j = 1, size(sol%y,2)
            if ( sol%y(i,j) < 0 ) then
                write(*,'(2(a,i0),a)') 'i = ', i, ', j = ', j, ', y<0, exiting'
                exit cl1
            end if
        end do
        !
        final_n = i
        !
    end do cl1
    !
    ! Write results
    !
    call write_output(n,final_n)
    !
    write(*,'(a)') 'Program compiled successfully.'
    write(*,'(a)') "Results stored in 'output.dat'"
    write(*,*)
    write(*,'(a)') "You can plot the results with 'make plot' or"
    write(*,'(a)') "'make allplots', and visualize them in graph/*.pdf"
    write(*,*)
    write(*,'(a)') 'Bye!'
    write(*,*)
    !
    stop
endprogram final_project
