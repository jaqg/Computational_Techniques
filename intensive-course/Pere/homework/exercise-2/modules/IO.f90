! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 10:18:14 25/01/2023 |
! +----------------------------------------------+
module io 
    ! 
    ! Input/Output module
    !
    implicit none
    !
    integer(4) :: i, j, uf, ifu, ios, istat
    real(kind=8) :: tolerance, ev
    real(kind=8), dimension(:), allocatable :: v 
    real(kind=8), dimension(3,3) :: A 

end module io 
