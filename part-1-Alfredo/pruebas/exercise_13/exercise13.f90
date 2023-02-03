! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Tuesday 13:06:31 04-10-2022  |
! +---------------------------------------------+

program exercise13
    use module1
    implicit none
    real(kind=8) :: a, b, c


    a = 2.0_8
    b = 3.0_8

    c = suma(a,b)
    write(*,*) 'c =', c

    stop
endprogram exercise13
