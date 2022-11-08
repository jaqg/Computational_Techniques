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
    real(kind=8), dimension(:), allocatable :: t, prey, predator
    real(kind=8) :: h, t0, tf, prey0, predator0
    real(kind=8) :: alpha, alphaprime, beta, kappa, kappaprime, lambda
    character(len=80) :: themodel, themethod
    !
    procedure(), pointer :: method => null()
    real(kind=8), external :: LV
    external :: Euler
    !
    ! Files
    !
    open(unit=10, file="input.dat", status='old')
    open(unit=11, file="output.dat")
    open(unit=12, file="graph/out-graph.dat")
    !
    !
    ! ========================= START OF THE PROGRAM =========================
    !
    !
    ! --- Read input ---
    !
    call read_input(10,themodel,themethod,h,tf,prey0,predator0,&
                  & alpha,alphaprime,beta,kappa,kappaprime,lambda)
    !
    ! Number of (time) steps
    !
    t0 = 0.0_8
    n = int( (tf - t0)/h )
    !
    ! Allocate arrays
    !
    call allocate_arrays(n, t, prey, predator)
    !
    ! Initial values
    !
    prey(1)     = prey0
    predator(1) = predator0
    !
    ! Set the pointer 'method' to the chosen method
    !
    if (themethod == "Euler" .or. themethod == "E") then
        method => Euler
    ! elseif (themethod == "Modified Euler" .or. themethod == "ME") then
    !     method => ModEuler
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
        call method( prey(i), h, &
                   & LV, kappa, -kappaprime, -lambda, prey(i), predator(i), &
                   & prey(i+1) )
        call method( predator(i), h, &
                   & LV, -alpha, alphaprime, beta, predator(i), prey(i), &
                   & predator(i+1) )
    end do lt1
    !
    ! Write results
    !
    call write_output(11,12,themodel,themethod,tf,h,n,final_n, &
                      & alpha,alphaprime,beta,kappa,kappaprime,lambda, &
                      & t,prey,predator)
    !
    write(*,*) 'Program compiled successfully.'
    stop
endprogram final_project
