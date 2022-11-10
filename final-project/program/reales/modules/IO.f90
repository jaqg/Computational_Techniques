! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 19:28:39 05-11-2022 |
! +---------------------------------------------+

module IO
    !
    ! Module to source data to the main program
    !
    implicit none
    !
    !
    ! The data is stored in the array 'y' of 'nsp' (number of species) columns.
    ! In this case:
    !
    ! y(i,1) -> prey
    ! y(i,2) -> predator
    !
    ! so nsp = 2
    !
    integer, parameter :: nsp = 2
    real(kind=8), dimension(:,:), allocatable :: y, yRk
    !
    ! We can write the Lotka-Volterra equations as:
    !
    ! dy/dt = f(t,y) = A * y + B * y**2 + C * x * y
    !
    ! where A, B, C are parameters (constants). The number of terms (addends)
    ! is nterms = 3.
    !
    integer, parameter :: nterms = 3
    !
    ! The parameters are stored in an array 'params' of nterms x nsp dimensions
    ! as
    !
    ! params(i,1) -> params of equation of prey
    ! params(i,2) -> params of equation of predator
    !
    real(kind=8), dimension(:,:), allocatable :: params
    !
    ! The time of the simulation is stored in a vector 't' of 'n' items
    !
    real(kind=8), dimension(:), allocatable :: t
    !
    ! Dummy variables
    !
    character(len=80) :: themodel, themethod
    real(kind=8) :: MEthreshold
    real(kind=8) :: t0, h, tf, prey0, predator0
    real(kind=8) :: alpha, alphaprime, beta, kappa, kappaprime, lambda
    !
    ! Procedures
    !
    contains
        !
        subroutine read_input
            !
            ! Subroutine to read input
            !
            implicit none
            !
            integer :: uf
            !
            open(newunit=uf, file="input.dat", action="read", status='old')
            !
            ! Model used
            !
            read(uf,*)
            read(uf,*) themodel
            themodel = trim(themodel)
            !
            ! Method used
            !
            read(uf,*)
            read(uf,*) themethod
            themethod = trim(themethod)
            !
            ! Threshold of the Modified Euler method (to cut iterating over n)
            !
            read(uf,*)
            read(uf,*) MEthreshold
            !
            ! Initial time (t0)
            !
            read(uf,*)
            read(uf,*) t0
            !
            ! Time step (h)
            !
            read(uf,*)
            read(uf,*) h
            !
            ! Time of the simulation
            !
            read(uf,*)
            read(uf,*) tf
            !
            ! Initial population of PREY (y_0)
            !
            read(uf,*)
            read(uf,*) prey0
            !
            ! Initial population of predator (x_0)
            !
            read(uf,*)
            read(uf,*) predator0
            !
            ! Constant ALPHA
            !
            read(uf,*)
            read(uf,*) alpha
            !
            ! Constant ALPHA PRIME
            !
            read(uf,*)
            read(uf,*) alphaprime
            !
            ! Constant BETA
            !
            read(uf,*)
            read(uf,*) beta
            !
            ! Constant KAPPA
            !
            read(uf,*)
            read(uf,*) kappa
            !
            ! Constant KAPPA PRIME
            !
            read(uf,*)
            read(uf,*) kappaprime
            !
            ! Constant LAMBDA
            !
            read(uf,*)
            read(uf,*) lambda
            !
            close(uf)
            return
        end subroutine read_input
        !
        subroutine allocate_arrays(n)
            !
            ! Subroutine to allocate arrays
            !
            implicit none
            !
            integer(kind=8), intent(in) :: n
            integer :: ierr
            !
            allocate(t(n), stat=ierr)
            if (ierr .ne. 0) stop 'IO.f90: Error in allocation of t'
            !
            allocate(params(nterms,nsp), stat=ierr)
            if (ierr .ne. 0) stop 'IO.f90: Error in allocation of params'
            !
            allocate(y(n+1,nsp), stat=ierr)
            if (ierr .ne. 0) stop 'IO.f90: Error in allocation of prey'
            !
            allocate(yRK(n+1,nsp), stat=ierr)
            if (ierr .ne. 0) stop 'IO.f90: Error in allocation of preyRK'
            !
            return
        end subroutine allocate_arrays
        !
        subroutine assign_params
            !
            ! Subroutine to assign desired parameters/variables for the
            ! main program
            !
            implicit none
            !
            ! Parameters (constants) of Lotka-Volterra model:
            ! kappa, kappaprime, lambda -> prey
            ! alpha, alphaprime, beta   -> predator
            !
            ! If the chosen model is the Simple one, then alphaprime and
            ! kappaprime must = 0 so the LV logistic model reduces to the
            ! simple one
            !
            if (themodel == "Simple" .or. themodel == "simple") then
                alphaprime = 0.0_8
                kappaprime = 0.0_8
            end if
            !
            params(:,1) = (/ kappa, -kappaprime, -lambda /)
            params(:,2) = (/ -alpha, alphaprime, beta /)
            !
            ! Initial values of species
            !
            y(1,1)   = prey0
            y(1,2)   = predator0
            yRK(1,1) = prey0
            yRK(1,2) = predator0
            !
            return
        end subroutine assign_params
        !
        subroutine write_output(n,final_n)
            !
            ! Subroutine to write output
            !
            implicit none
            !
            integer :: uf, uf2
            integer(kind=8), intent(in) :: n, final_n
            integer(kind=8) :: i, j
            !
            ! Files
            !
            open(newunit=uf, file="output.dat")
            open(newunit=uf2, file="graph/out-graph.dat")
            !
            ! Formats
            !
            999 format('------------------------------------------------')
            899 format(a, 1x, f10.6)
            !
            write(uf,'(a)') '+-------------------------------------+'
            write(uf,'(a)') '|        Program final_project        |'
            write(uf,'(a)') '| Author: Jose Antonio Quinonero Gris |'
            write(uf,'(a)') '+-------------------------------------+'
            write(uf,*)
            !
            themodel = trim(themodel)
            write(uf,'(a,1x,a)') 'Model:', themodel
            !
            themethod = trim(themethod)
            write(uf,'(a,1x,a)') 'Method:', themethod
            write(uf,*)
            !
            write(uf,'(a,1x,f10.2)') 'Initial time, t_0 (arbitrary units):', t0
            write(uf,'(a,1x,f10.2)') 'Time of simulation (arbitrary units):',tf
            write(uf,'(a,1x,f10.2)') 'Time step (arbitrary units):', h
            write(uf,'(a,1x,i0)') 'Number of points:', n
            !
            ! Print value of constants
            !
            write(uf,*)
            write(uf,'(a)') 'Constants:'
            write(uf,899) 'alpha =', alpha
            write(uf,899) 'alphaprime =', alphaprime
            write(uf,899) 'beta =', beta
            write(uf,899) 'kappa =', kappa
            write(uf,899) 'kappaprime =', kappaprime
            write(uf,899) 'lambda =', lambda
            write(uf,*)
            !
            write(uf,'(a)') 'Results:'
            write(uf,'(a,1x,i0)') 'Number of iterations:', final_n
            write(uf,'(a,1x,f15.2)') 'Total time of simulation:', t(final_n)
            write(uf,*)
            write(uf,999)
            write(uf,'(11x, "t", 12x, "Prey", 11x, "Predator")')
            write(uf2,'(a)') themethod
            write(uf2,'(11x, "t", 12x, "Prey", 11x, "Predator")')
            write(uf,999)
            !
            lw1: do i = 1, final_n
                write(uf,'(*(f15.2))') t(i), ( y(i,j), j=1,nsp )
                ! write(uf2,'(*(f15.2))') t(i), ( y(i,j), j=1,nsp )
                write(uf2,'(*(f15.2))') t(i), ( y(i,j), j=1,nsp ), &
                                            & ( yRK(i,j), j=1,nsp )
            end do lw1
            !
            close(uf)
            close(uf2)
            return
        end subroutine write_output
end module
