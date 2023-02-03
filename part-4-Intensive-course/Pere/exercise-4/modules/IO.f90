! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Friday 15:46:47 20/01/2023 |
! +-------------------------------------------+

module io 
    ! 
    ! Input/Output module
    !
    implicit none
    integer :: i, j, ios, uf, ifu, niter
    real(kind=8) :: eval, threshold
    character(len=80) :: rfmt 
    real(kind=8), dimension(:,:), allocatable :: A
    real(kind=8), dimension(:), allocatable :: v, vp
    !
    end module io 
