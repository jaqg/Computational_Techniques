! =========================================================================================
!          Romberg Method for Integration of a Function (Integration Subroutines)
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
    public :: int_romberg

    contains

    subroutine int_romberg(f, a, b, k, R)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to calculate the integral o a function using the Romberg Method
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f: the function to be integrated
    ! - a: the lower limit of the integral
    ! - b: the upper limit of the integral
    ! - k: number of k intervals to use
    ! Outputs
    ! - R: matrix with the iterations over k and k
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: k
        real(kind=8), intent(in) :: a, b
        real(kind=8), allocatable, intent(out) :: R(:,:)

        ! Internal variables --------------------------------------------------------------
        integer :: i, j, t
        real(kind=8) :: f, temp
        real(kind=8), allocatable :: h(:)

        ! Assigning memory to R matrix and h vector ---------------------------------------
        allocate(R(k,k))
        allocate(h(k))

        R = 0
        h = 0

        ! Computing the first values of the Romberg matrix --------------------------------
        h(1) = (b - a)
        R(1,1) = 0.5 * h(1) * (f(a) + f(b))

        ! Computing the integral ----------------------------------------------------------
        do i=2, k
            h(i) = (b - a)/2**(i - 1)                          ! Computing h(k)
            do j=1, i
                if (j == 1) then                               ! If first entry in row ...
                    temp = 0
                    do t=1, 2**(i - 2)
                        temp = temp + f(a + (2*t - 1) * h(i))  ! ... compute the sum in R
                    end do
                    R(i,j) = 0.5 * ( R(i-1,j) + h(i-1) * temp )! And then compute R
                else                                           ! Otherwise ...
                                                               ! ... compute R based on
                                                               ! previous values of R
                    R(i,j) = R(i, j-1) + (R(i, j-1) - R(i-1, j-1)) / (4**(j-1) - 1)
                end if
            end do
        end do

    end subroutine int_romberg

end module imethods