! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 13:24:21 26/02/2023 |
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
        ! $\sin(x^2) - \cos(2x)$
        !
        f = dsin(x**2) - dcos(2.0_8 * x)
        !
        return
    end function f

    SUBROUTINE sub_GauLeg(n,t,w)
        !Gauss-Legendreren subroutine. 
        !! It gives t and w values

        !Modification of 
        !https://github.com/NREL/OpenWARP/blob/master/source/NemohImproved/Nemoh/Solver/Core/Gaussm3.f90
        !* Calculation of GAUSS-LEGENDRE abscissas and weights for Gaussian Quadrature
        !* integration of polynomial functions.
        !*      For normalized lower and upper limits of integration -1.0 & 1.0, and
        !* given n, this routine calculates, arrays t(1:n) and  w(1:n) of length n,
        !* containing the abscissas and weights of the Gauss-Legendre n-point quadrature
        !* formula. 
        !* For detailed explanations finding weights & abscissas, see  "Numerical Recipes in Fortran */
        !
        !otra forma de eliminar X1 y X2, y XL y XM
        !sera t(i) = -z y t(n+1-i)= +z
        !
        ! X1: lower integration limit
        ! X2: upper integration limit

        IMPLICIT NONE

        INTEGER :: m,i,j
        INTEGER,intent(in) :: n   !Number of Gaussian points
        REAL*8, dimension(:), allocatable, intent(out) :: W,t
        REAL*8 :: XM,XL,X1,X2,EPS,P1,P2,P3,pi,Z1,Z,PP
        integer :: ierr 

        ! Allocate arrays
        allocate(W(n), stat=ierr)
        if (ierr .ne. 0) stop 'sub_GauLeg: Error in allocation of W'
        allocate(t(n), stat=ierr)
        if (ierr .ne. 0) stop 'sub_GauLeg: Error in allocation of t'
        
        !Relative precision
        EPS = 1.D-14

        !double precision arccosine. Pi value=3.14159
        pi = DACOS(-1.D0)

        !N = number of Gauss Points
        !Roots are symmetric in the interval - so only need to find half of them  
        m = (n + 1) / 2

        !The coats are going to be  -1 and 1, Gauss-Legendre 
        X1 = -1.0_8
        X2 = 1.0_8
        !Variable change
        XM=0.5D0*(X1+X2) ! c
        XL=0.5D0*(X2-X1) ! m

        !Loop over the desired roots
        DO i = 1,m
            Z = DCOS (pi * (i - 0.25D0)/(n + 0.5D0))
            !Starting with the above approximation to the i-th root,
            !we enter the main loop of refinement by NEWTON'S method   
            10      P1 = 1.D0
            P2 = 0.D0

            !Loop up the recurrence relation to get the Legendre
            !polynomial evaluated at z                
            DO j = 1,n
                P3 = P2
                P2 = P1
                P1 = ((2.D0 * j - 1.D0) * Z * P2 - (j - 1.D0) * P3)/j
            END DO
            !p1 is now the desired Legendre polynomial.
            !We next compute pp, its derivative, by a standard relation involving also p2, 
            !the polynomial of one lower order. 
            PP = n * (Z * P1 - P2)/(Z * Z - 1.D0)
            Z1 = Z
            Z = Z1 - P1/PP  ! Newton's Method

            IF (DABS(Z-Z1) .GT. EPS) GO TO 10

            ! Roots will be symmetric about the origin  
            t(i) = XM - XL * Z
            t(n + 1 - i) = XM + XL * Z
            !Compute the weight and its symmetric counterpart 
            W(i) = 2.D0 * XL/((1.D0 - Z * Z) * PP * PP)
            W(n + 1 - i) = W(i)
        END DO  

        RETURN   !not neccesary
    END SUBROUTINE Sub_GauLeg

    subroutine GaussQuadrature(lil,uil,initN,totN,threshold,IRC,totiter,uf)
        !
        ! Subroutine for the Gauss Quadrature method
        !
        ! lil           real, doule precision
        ! uil           real, doule precision
        ! initN         integer, single precision
        ! totN          integer, single precision
        ! threshold     real, double precision
        ! IRC           real, doule precision
        ! totiter       integer, doule precision
        !
        ! lil: lower integration limit
        ! uil: upper integration limit
        ! initN: number of quadrature points (n) to start with
        ! totN: total number of subintervals (n)
        ! threshold: threshold for convergence
        ! IRC: result of the integral
        ! totiter: total number of iterations
        !
        implicit none
        !
        real(kind=8), intent(in) :: lil, uil, threshold
        integer, intent(in) :: uf, initN, totN
        real(kind=8), intent(out) :: IRC
        integer(kind=8), intent(out) :: totiter
        ! Dummy variables
        integer :: i, n
        real(kind=8) :: prevIRC, diffIRC, xtmp, m, c
        real(kind=8), dimension(:), allocatable :: wG, tG 
        !
        ! Initialize variables
        !
        prevIRC = 0.0_8
        !
        ! Calculate c = (a + b)/2 and m = (b - a)/2
        !
        c = (uil + lil)/2.0_8
        m = (uil - lil)/2.0_8
        !
        ! Header of the table
        !
        write(uf,'(A,3x,A,3x,A)') &
        & 'Quadrature pts', 'Integral value', 'Difference'
        !
        ! Main loop over number of subintervals
        !
        lcr1: do n=initN, totN
            !
            ! Compute the weights and t_i values arrays form the given subroutine
            !
            call sub_GauLeg(n,tG,wG)
            !
            ! Compute the integral as
            !
            ! ICR = m sum_{i=1}^n w_i f(t_i)
            !     = m sum_{i=1}^n w_i f(c + m*t_i)
            !
            ! First the sums: xtmp = sum_{i=1}^n w_i f(t_i)
            ! And then the final value: IRC = m * xtmp
            !
            ! Initialize variables
            !
            xtmp = 0.0_8
            !
            ! Compute the sum
            !
            lcr2: do i = 1, n
                !
                xtmp = xtmp + wG(i) * f( c + m * tG(i) )
                !
            end do lcr2
            ! write(*,'(a,f10.4)') 'Result of sum: ', xtmp
            !
            ! Final value of IRC
            !
            IRC = m * xtmp
            !
            ! Calculate the difference with the previous value of IRC
            !
            diffIRC = dabs(IRC - prevIRC)
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
            write(uf,'(1(i10, 4x), 2(f13.8, 4x))') n, IRC, diffIRC
            !
            ! Total number of iterations
            !
            totiter = n
            !
        end do lcr1
        !
        ! Print results
        !
        write(uf,*)
        write(uf,'(A, i0)') 'Number of quadrature points employed: ', totiter
        write(uf,'(A, f13.8)') 'Final value of the quadrature: ', IRC
        !
        return
    end subroutine GaussQuadrature

end module
