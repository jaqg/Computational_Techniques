! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 00:48:51 18-12-2022 |
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
            ! Function to find the roots of
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
            ! f = e^{-x} (3.2 sin(x) - 0.5 cos(x))
            !
            f = dexp(-x) * ( 3.2_8 * dsin(x) - 0.5_8 * dcos(x) )
            !
            return
        end function f
        !
        real(kind=8) function c_bisection_method(a, b)
            !
            ! Function to calculate the value of 'c' in the Bolzano's
            ! algorithm
            !
            ! c_bisection_method     real, double precision
            ! a                      real, double precision
            ! b                      real, double precision
            !
            ! c_bisection_method: result of the function
            ! a: lower limit of the interval [a,b] of the Bolzano's algorithm
            ! b: upper limit of the interval [a,b] of the Bolzano's algorithm
            !
            implicit none
            real(kind=8), intent(in) :: a, b
            !
            c_bisection_method = ( a + b )/2.0_8
            !
        end function c_bisection_method
        !
        real(kind=8) function c_regula_falsi_method(a, b)
            !
            ! Function to calculate the value of 'c' in the Bolzano's
            ! algorithm
            !
            ! c_regula_falsi_method     real, double precision
            ! a                         real, double precision
            ! b                         real, double precision
            !
            ! c_regula_falsi_method: result of the function
            ! a: lower limit of the interval [a,b] of the Bolzano's algorithm
            ! b: upper limit of the interval [a,b] of the Bolzano's algorithm
            !
            implicit none
            real(kind=8), intent(in) :: a, b
            !
            c_regula_falsi_method = ( a * f(b) - b * f(a) )/( f(b) - f(a) )
            !
        end function c_regula_falsi_method
        !
        subroutine Bolzano(method, a, b, threshold, res, totiter)
            !
            ! Subroutine to find the roots of a one-dimensional function 'f'
            ! with the Bolzano's algorithm and the
            ! - Bisection method
            ! - Regula Falsi method
            !
            ! method            character
            ! a                 real, double precision
            ! b                 real, double precision
            ! threshold         real, double precision
            ! res               real, double precision
            ! totiter           integer, single precision
            !
            ! method: Bisection or Regula Falsi method to calculate 'c'
            ! a: lower limit of the interval [a,b] of the Bolzano's algorithm
            ! b: upper limit of the interval [a,b] of the Bolzano's algorithm
            ! threshold: threshold of the convergence
            ! res: root of the function
            ! totiter: total number of iterations
            !
            implicit none
            !
            character(len=*) :: method
            real(kind=8), intent(in) :: a, b
            real(kind=8), intent(in) :: threshold
            real(kind=8), intent(out) :: res
            integer, intent(out) :: totiter
            !
            ! Dummy variables
            !
            integer :: i
            real(kind=8) :: c, x1, x2
            procedure( real(kind=8) ), pointer :: c_method
            !
            ! Select the method (Bisection or Regula Falsi) from the character
            ! input variable 'method'
            !
            c_method => null()
            !
            if (method=='Bisection' .or. method=='bisection') then
                c_method => c_bisection_method
            elseif (method=='Regula_Falsi' .or. method=='regula_falsi') then
                c_method => c_regula_falsi_method
            end if
            !
            ! Two dummy variables, x1 and x2 are asign as the interval
            ! [a,b] -> [x1,x2]
            ! to update through the iterations
            !
            x1 = a
            x2 = b
            !
            ! Main loop
            !
            ! Initialize dummy variable 'i' as the number of iterations
            !
            i = 0
            !
            dl1: do
                !
                ! Update dummy variable 'i' as the number of iterations
                !
                i = i + 1
                !
                ! Choose a value for 'c'
                !
                c = c_method(x1, x2)
                !
                ! Condition to update the interval [a,b] or end the program
                !
                ifc1: if (abs(f(c)) < threshold ) then
                    res = c
                    exit dl1
                elseif( f(c) > 0 ) then ! the root is in the interval [a,c]
                    x2 = c
                elseif( f(c) < 0 ) then ! the root is in the interval [c,b]
                    x1 = c
                end if ifc1
                !
            end do dl1
            !
            ! Output total number of
            !
            totiter = i
            !
            return
        end subroutine Bolzano
end module
