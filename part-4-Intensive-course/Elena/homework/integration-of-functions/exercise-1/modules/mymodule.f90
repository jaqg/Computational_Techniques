! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 23:04:58 25/02/2023 |
! +---------------------------------------------+

module mymodule
    !
    ! Module to contain my functions/subroutines
    !
    implicit none
    !
    ! Subprograms
    !
    contains
    !
    real(kind=8) function f(x)
        !
        ! Function to integrate
        !
        ! f     real, double precision
        ! x     real, double precision
        !
        ! f: result of the function
        ! x: independent variable
        !
        implicit none
        real(kind=8), intent(in) :: x
        !
        ! $\sin(x^2) - \cos(2x)$
        !
        f = dsin(x**2) - dcos(2.0_8 * x)
        !
        return
    end function f

    subroutine SimpsonCompositeNCM(lil,uil,initN,threshold,IRC,totiter,uf)
        !
        ! Subroutine for the composite Simpson rule of the Newton-Cotes 
        ! Method (NCM)
        !
        ! lil           real, doule precision
        ! uil           real, doule precision
        ! initN         integer, single precision
        ! IRC           real, doule precision
        ! totiter       integer, doule precision
        !
        ! lil: lower integration limit
        ! uil: upper integration limit
        ! initN: number of subintervals (N) to start with
        ! IRC: result of the integral
        ! totiter: total number of iterations
        !
        implicit none
        !
        real(kind=8), intent(in) :: lil, uil, threshold
        integer, intent(in) :: uf, initN
        real(kind=8), intent(out) :: IRC
        integer(kind=8), intent(out) :: totiter
        ! Dummy variables
        integer :: i, j, iterstep
        real(kind=8) :: h, prevIRC, diffIRC, xtmp, ytmp, xi
        !
        ! Initialize variables
        !
        prevIRC = 0.0_8
        iterstep = 0
        j = initN
        !
        ! Header of the table
        !
        write(uf,'(A,7x,A,3x,A,6x,A)') &
        & 'Iter. step', 'Subinterval', 'Integral value', &
        & 'Difference'
        !
        ! Main loop over number of subintervals
        !
        lcr1: do
            !
            ! Iteration step
            !
            iterstep = iterstep + 1
            !
            ! Calculate value of h
            !
            h = (uil - lil)/(2.0_8 * dble(j))
            !
            ! Compute ICR = h/3 * [ f(a) + f(b) + 
            !               2 sum_{even i=2}^{2N-2} f(x_i) +
            !               4 sum_{odd i=1}^{2N-1} f(x_i) ]
            !
            ! First the sums:
            !  xtmp = sum_{even i=2}^{2N-2} f(x_i)
            !  ytmp = sum_{odd i=1}^{2N-1} f(x_i)
            ! And then the final value: IRC = h/3 * ( f(a)+f(b)+2*xtmp+4*ytmp )
            !
            ! Initialize variables
            !
            xtmp = 0.0_8
            ytmp = 0.0_8
            !
            lcr2: do i = 2, 2*j-2, 2
                !
                ! Value of x_i = a + ih
                !
                xi = lil + dble(i) * h
                !
                xtmp = xtmp + f(xi)
                !
            end do lcr2
            !
            lcr3: do i = 1, 2*j-1, 2
                !
                ! Value of x_i = a + ih
                !
                xi = lil + dble(i) * h
                !
                ytmp = ytmp + f(xi)
                !
            end do lcr3
            !
            ! Final value of IRC
            !
            IRC = h/3.0_8 * ( f(lil) + f(uil) + 2.0_8 * xtmp + 4.0_8 * ytmp)
            !
            ! Calculate the difference with the previous value of IRC
            !
            diffIRC = abs(IRC - prevIRC)
            !
            ! Print results
            !
            ! write(uf,'(2(i10, 4x), 2(f13.8, 4x))') j, i, IRC, diffIRC
            !
            ! If difference < threshold of convergence, exit
            !
            if (diffIRC < threshold .and. diffIRC > 0.0_8) exit lcr1
            !
            ! Update prevIRC
            !
            prevIRC = IRC
            !
            ! Print results
            !
            write(uf,'(1(i10, 4x), 3(f13.8, 4x))') iterstep, h, IRC, diffIRC
            !
            ! Iterate over j
            !
            j = 2 * j
            !
        end do lcr1
        !
        ! Total number of iterations
        !
        totiter = iterstep
        !
        ! Print results
        !
        write(uf,*)
        write(uf,'(A, i0)') 'Number of iterations needed: ', totiter
        write(uf,'(A, f13.8)') 'Final subinterval value: ', h
        write(uf,'(A, i0)') 'Number of abscissa points: ', j
        write(uf,'(A, f13.8)') 'Final value of the quadrature: ', IRC
        !
        return
    end subroutine SimpsonCompositeNCM
end module
