module random_numbers
    implicit none
    contains
        function random_integer(n, m)
            !
            ! Generates a random integer number in the discrete uniform
            ! distribution {n, n+1, ..., m-1, m}
            !
            ! n (integer): lower limit of the distribution
            ! m (integer): upper limit of the distribution
            !
            implicit none
            !
            integer, intent(in) :: n, m
            integer :: random_integer
            real(kind=8) :: u
            !
            call random_number(u)
            random_integer = n + FLOOR((m+1-n)*u)
            !
            return
        end function random_integer
end module
