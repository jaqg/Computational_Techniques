! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Monday 15:37:52 16/01/2023 |
! +-------------------------------------------+

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
        ! f = 1/( 1 + x )
        !
        f = 1/( 1 + x )
        !
        return
    end function f
    !
    subroutine CompositeNCTM(lil, uil, initN, threshold, IRC, totiter, uf)
        !
        ! Subroutine for the composite trapezoidal rule of the Newton-Cotes 
        ! Trapezoid Method (NCRM)
        !
        ! lil           real, doule precision
        ! uil           real, doule precision
        ! initN         integer, single precision
        ! IRC           real, doule precision
        ! totiter       integer, doule precision
        !
        ! lil: lower integration limit
        ! uil: upper integration limit
        ! initN: number of subintervals (N) to start
        ! IRC: result of the integral
        ! totiter: total number of iterations
        !
        implicit none
        !
        real(kind=8), intent(in) :: lil, uil, threshold
        integer, intent(in) :: uf, initN
        real(kind=8), intent(out) :: IRC
        integer(kind=8), intent(out) :: totiter
        integer :: i, j, iterstep
        real(kind=8) :: h, prevIRC, diffIRC, xtmp, xi
        !
        ! Intro message
        !
        write(uf,'(A)') 'COMPOSITE RULE'
        write(uf,*)
        !
        ! Initialize variables
        !
        prevIRC = 0.0_8
        iterstep = 0
        j = initN/2
        !
        ! Header of the table
        !
        write(uf,'(A,7x,A,3x,A)') &
        & 'Iter. step', 'Integral value', &
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
            ! Iterate over j
            !
            j = 2 * j
            !
            ! Calculate value of h
            !
            h = (uil - lil)/dble(j)
            !
            ! Compute ICR = h/2 * [ f(a) + f(b) + 2 sum_{i=1}^{N-1} f(x_i) ]
            !
            ! First the sum: xtmp = sum_{i=1}^{N-1} f(x_i)
            ! And then the final value: IRC = h/2 * ( f(a) + f(b) + 2 * xtmp )
            !
            ! Initialize variables
            !
            xtmp = 0.0_8
            !
            lcr2: do i = 1, j-1
                !
                ! Value of x_i = a + ih
                !
                xi = lil + dble(i) * h
                !
                xtmp = xtmp + f(xi)
                !
            end do lcr2
            !
            ! Final value of IRC
            !
            IRC = h/2 * ( f(lil) + f(uil) + 2 * xtmp )
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
            write(uf,'(1(i10, 4x), 2(f13.8, 4x))') iterstep, IRC, diffIRC
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
    end subroutine CompositeNCTM
end module
