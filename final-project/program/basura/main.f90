! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Wednesday 18:17:00 26-10-2022 |
! +-------------------------------------------+

program main
    !
    !
    !
    ! IMPLICIT REAL*8(A-H,O-Z)
    ! IMPLICIT INTEGER(I-N)
    !
    ! Variable definition
    !
    implicit none
    !
    real(kind=8) :: a
    integer :: i
    !
    ! Files
    !
    !
    ! Formats
    !
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+------------------+'
    write(*,*) '| Program main |'
    write(*,*) '+------------------+'
    write(*,*)
    !
    write(*,*) '2*2 =', 2*2
    write(*,*) '2.0*2 =', 2.0*2
    write(*,*)
    a = 0.02_8
    i = 30
    write(*,*) 'a =', a, 'i =', i
    write(*,*) 'a * i =', a*i
    write(*,*) 'int(a) * i =', int(a)*i
    write(*,*) 'a * dble(i) =', a*dble(i)
    write(*,*) 'int(a * i) =', int(a*i)
    !
    stop
endprogram main
