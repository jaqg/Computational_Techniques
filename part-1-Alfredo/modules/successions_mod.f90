module successions_mod
contains
    function sopoinn(n,k)
        !
        ! Succession of powers of inverse natural numbers: 1/n^k
        !
        ! n (integer), k (integer)
        !
        implicit none
        real(kind=8) :: sopoinn
        integer, intent(in) :: n,k
        !
        sopoinn = 1.0_8/dble(n)**k
        !
        return
    end function sopoinn
    !
    subroutine ssopoinn(n, k, suma)
        !
        ! Summing succession of powers of inverse natural numbers
        !
        ! n (integer): number of terms to be sumed
        ! k (integer): power (exponent)
        ! suma (real*8): suma = sum_{i=1}^n 1/(i**k)
        !
        implicit none
        integer, intent(in) :: n, k
        real(kind=8), intent(out) :: suma
        integer :: i
        !
        suma = 0.0_8
        loop1: do i = 1, n
            suma = suma + sopoinn(i,2)
        end do loop1
        !
        return
    end subroutine ssopoinn
end module
