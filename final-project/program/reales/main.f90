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
    interface
        function func(t, y) result(res)
            implicit none
            real(kind=8), optional, intent(in) :: t
            real(kind=8), dimension(:), intent(in) :: y
            real(kind=8), dimension(:), allocatable :: res
        end function func
        subroutine methods(f, y0, t0, tf, h, t, y)
            implicit none
            procedure(func) :: f
            real(kind=8), dimension(:), intent(in) :: y0
            real(kind=8), intent(in) :: t0, tf, h
            real(kind=8), dimension(:), allocatable, intent(out) :: t
            real(kind=8), dimension(:,:), allocatable, intent(out) :: y
        end subroutine methods
        subroutine methods2(f, y0, t0, tf, h, threshold, t, y)
            implicit none
            procedure(func) :: f
            real(kind=8), dimension(:), intent(in) :: y0
            real(kind=8), intent(in) :: t0, tf, h, threshold
            real(kind=8), dimension(:), allocatable, intent(out) :: t
            real(kind=8), dimension(:,:), allocatable, intent(out) :: y
        end subroutine methods2
    end interface
    !
    procedure(func) :: LV
    procedure(methods), pointer :: method => null()
    procedure(methods) :: Euler, RK4
    procedure(methods2) :: modEuler
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
    ! call allocate_arrays
    !
    ! Set the pointer 'method' to the chosen method
    !
    ! if (themethod == "Euler" .or. themethod == "E") then
    !     method => Euler
    ! elseif (themethod == "ModEuler" .or. themethod == "ME") then
    !     method => modEuler
    ! elseif (themethod == "Taylor" .or. themethod == "T") then
    !     method => Taylor
    ! elseif (themethod == "Runge-Kutta" .or. themethod == "RK4") then
    !     method => RK4
    ! else
    !     write(*,*) 'main.f90 ERROR: wrong method input'
    ! end if
    !
    ! Assign needed parameters/variables
    !
    call assign_params
    !
    ! Main calculation
    !
    ! Calculate Runge-Kutta for comparison
    !
    call RK4( LV, y0, t0, tf, h, t, yRK )
    !
    if (themethod == "Euler" .or. themethod == "E") then
        write(*,*) 'The method is Euler'
        call Euler( LV, y0, t0, tf, h, t, y )
    elseif (themethod == "ModEuler" .or. themethod == "ME") then
        write(*,*) 'The method is Modified Euler'
        call modEuler( LV, y0, t0, tf, h, MEthreshold, t, y )
    elseif (themethod == "Taylor" .or. themethod == "T") then
        write(*,*) 'The method is Taylor series'
    elseif (themethod == "Runge-Kutta" .or. themethod == "RK4") then
        write(*,*) 'The method is 4th order Runge-Kutta'
        y = yRK
    else
        write(*,*) 'main.f90 ERROR: wrong method input'
    end if
    !
    ! call method( LV, y0, t0, tf, h, t, y )
    !
    ! If any individual becomes <0, stop
    !
    do i = 1, size(y,1)
        !
        do j = 1, size(y,2)
            if ( y(i,j) < 0 ) write(*,*) 'y<0, exiting' ; exit
        end do
        !
        final_n = i
        !
    end do
    !
    ! Write results
    !
    call write_output(n,final_n)
    !
    write(*,*) 'Program compiled successfully.'
    stop
endprogram final_project
