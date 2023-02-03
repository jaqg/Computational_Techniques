module module1
contains
    function suma(x,y)
        implicit none
        real(kind=8) :: suma
        real(kind=8) , intent(in) :: x,y

        suma = x + y

    end function suma
end module
