! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 10:18:14 25/01/2023 |
! +----------------------------------------------+
module io 
    ! 
    ! Input/Output module
    !
    use mymodule
    !
    implicit none
    !
    integer :: i, j, uf, ifu, ios, istat, n, totiter
    real(kind=8) :: tolerance
    real(kind=8), dimension(:,:), allocatable :: v 
    real(kind=8), dimension(:), allocatable :: ev
    real(kind=8), dimension(3,3) :: A 

    contains

    subroutine read_input
        !
        open(newunit=ifu, file='./data/input.dat', iostat=ios, status="old", &
        & action="read")
        if (ios /= 0) stop "Error opening file 'data/input.dat'"
        !
        read(ifu,*)
        read(ifu,*) tolerance
        !
        close(ifu)
        !
        return
    end subroutine read_input

    subroutine print_input
        !
        write(unit=uf, fmt='(a)') '=== INPUT ==='
        write(unit=uf, fmt=*)
        !
        write(unit=uf, fmt='(A)') 'Input matrix:'
        call write_mat(A, uf, 'f10.5', size(A, dim=1)) 
        write(unit=uf, fmt=*)
        !
        write(unit=uf, fmt='(A,d10.2)') 'Tolerance for the Jacobi algorithm: ', &
        & tolerance
        write(unit=uf, fmt=*)
        !
        return
    end subroutine print_input 

    subroutine print_output
        !
        write(unit=uf, fmt='(a)') '=== RESULTS ==='
        write(unit=uf, fmt=*)
        !
        write(unit=uf,fmt='("Number of iterations: ", i0)') totiter
        write(unit=uf, fmt=*)
        !
        write(unit=uf,fmt='(a)') 'Eigenvalues:'
        do i = 1, n
            write(unit=uf,fmt='(*(f15.5))') ev(i)
        end do
        write(unit=uf,fmt=*)
        !
        write(unit=uf,fmt='(a)') 'Normalised eigenvector matrix V'
        do i = 1, n
            write(unit=uf,fmt='(*(f15.5))') ( V(i,j), j=1, n )
        end do
        write(unit=uf,fmt=*)
        !
        return
    end subroutine print_output 

end module io 
