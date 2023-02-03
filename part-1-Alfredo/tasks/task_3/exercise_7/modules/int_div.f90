function int_div(n1, n2) result(res)
    !
    ! Converts into an integer by using INT the result of the division
    ! of two (real) numbers, n1 and n2
    !
    implicit none
    !
    real(kind=8), intent(in) :: n1, n2
    integer :: res
    !
    res = int(n1/n2)
    !
    return
end function int_div
