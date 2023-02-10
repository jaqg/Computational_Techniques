! =========================================================================================
!                Simpson Method for Integration (Integration Subroutines)
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

module imethods
    implicit none
    
    ! Establishing the subroutines that will be provided to the external program
    public :: int_simp

    contains

    subroutine int_simp(f, a, b, n, Is)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to calculate the integral o a function using the Simpson Method
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f: the function to be integrated
    ! - a: the lower limit of the integral
    ! - b: the upper limit of the integral
    ! - n: number of parabolas to construct the area under the curve (intervals)
    ! Outputs
    ! - Is: the area under the curve for function f, between a and b
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: n
        real(kind=8), intent(in) :: a, b
        real(kind=8), intent(out) :: Is

        ! Internal variables --------------------------------------------------------------
        integer :: i
        real(kind=8) :: f, delta, area

        ! Setting the initial area and computing the length of the intervals --------------
        delta = (b - a) / (2*n)
        area = f(a) + f(b)

        ! Computing the area by adding the area under the curve of each parabola ----------
        do i = 1, (2*n) - 1
            area = area + 2**(mod(i,2) + 1) * f(a + i * delta)
        end do
        Is = area * delta/3

    end subroutine int_simp

end module imethods