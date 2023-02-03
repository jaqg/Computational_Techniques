! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Friday 17:16:20 09-12-2022 |
! +-------------------------------------------+

module mymodule
    !
    ! Module to contain my functions/subrtouines
    !
    implicit none
    !
    ! Subprograms
    !
    contains
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
            ! f = sin(x + 1)
            !
            f = dsin( x + 1.0_8 )
            !
            return
        end function f
        !
        subroutine SimpleNCRM(lil, uil, IR, uf)
            !
            ! Subroutine of the Simple rule of the Newton-Cotes Rectangle
            ! Method (NCRM)
            !
            ! lil       real, doule precision
            ! uil       real, doule precision
            ! IR        real, doule precision
            !
            ! lil: lower integration limit
            ! uil: upper integration limit
            ! IR: result of the integral
            !
            implicit none
            real(kind=8), intent(in) :: lil, uil
            real(kind=8), intent(out) :: IR
            integer, intent(in) :: uf
            real(kind=8) :: h
            !
            ! Intro message
            !
            write(uf,'(A)') 'SIMPLE RULE'
            write(uf,*)
            !
            ! Calculate value of h
            !
            h = uil - lil
            !
            write(uf,*) 'h =', h
            !
            ! Compute IR = f(a) * h -> f(lower int. lim.=lil) * h
            !
            IR = f(lil) * h
            !
            write(uf,*) 'IR =', IR
            write(uf,*)
            !
            return
        end subroutine SimpleNCRM
        !
        subroutine CompositeNCRM(lil, uil, threshold, IRC, uf)
            !
            ! Subroutine of the Simple rule of the Newton-Cotes Rectangle
            ! Method (NCRM)
            !
            ! lil       real, doule precision
            ! uil       real, doule precision
            ! IR        real, doule precision
            !
            ! lil: lower integration limit
            ! uil: upper integration limit
            ! IR: result of the integral
            !
            implicit none
            real(kind=8), intent(in) :: lil, uil, threshold
            integer, intent(in) :: uf
            real(kind=8), intent(out) :: IRC
            integer :: i, j
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
            j = 0
            !
            ! Header of the table
            !
            write(uf,'(A,3x,A,5x,A,3x,A)') &
                & 'Iter. step', 'Subint. numb.', 'Integral value', &
                & 'Difference'
            !
            ! Main loop over number of subintervals
            !
            lcr1: do
                !
                ! Iterate over j
                !
                j = j + 1
                !
                ! Calculate value of h
                !
                h = (uil - lil)/dble(j)
                !
                ! Compute ICR = h * sum_{i=1}^{N} f(x_i)
                !
                ! First the sum: xtmp = sum_{i=1}^{N} f(x_i)
                ! And then the final value: IRC = h * xtmp
                !
                ! Initialize variables
                !
                xtmp = 0.0_8
                !
                lcr2: do i = 1, j
                    !
                    ! Value of x_i = a + (i - 1) * h
                    !
                    xi = lil + dble(i - 1) * h
                    !
                    xtmp = xtmp + f(xi)
                    !
                    ! Final value of IRC
                    !
                    IRC = h * xtmp
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
                end do lcr2
                !
                ! Update prevIRC
                !
                prevIRC = IRC
                !
                ! Print results
                !
                write(uf,'(1(i10, 4x), 2(f13.8, 4x))') j, IRC, diffIRC
                !
            end do lcr1
            !
            return
        end subroutine CompositeNCRM
end module
