module fibonacci_series
    implicit none
    contains
        function fibo_n(n)
            !
            ! Calculate the nth fibonacci (integer) number defined by the
            ! recurrence relation
            ! F_n = F_{n-1} + F_{n-2}
            ! with the inital values F_0 = 0 and F_1 = 1
            !
            implicit none
            !
            integer, intent(in) :: n
            real(kind=8) :: fibo_n, fn1, fn2
            integer :: i
            !
            fn2 = 0
            fn1 = 1
            do i = 2, n
                fibo_n = fn1 + fn2
                fn2 = fn1
                fn1 = fibo_n
            end do
            !
            return
        end function fibo_n
        !
        function aux_fibo_n(fn1, fn2)
            !
            ! Calculate the nth fibonacci (integer) number defined by the
            ! recurrence relation
            ! F_n = F_{n-1} + F_{n-2}
            ! with the inital values F_0 = 0 and F_1 = 1
            !
            implicit none
            !
            integer(kind=8), intent(in) :: fn1, fn2
            real(kind=8) :: aux_fibo_n
            !
            aux_fibo_n = fn1 + fn2
            !
            return
        end function aux_fibo_n
end module
