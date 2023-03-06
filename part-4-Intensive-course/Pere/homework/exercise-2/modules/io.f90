! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 10:18:14 25/01/2023 |
! +----------------------------------------------+
module io 
    ! 
    ! Input/Output module
    !
    use amymodule
    !
    implicit none
    !
    integer :: i, j, uf, ifu, ios, istat, n, totiter, natoms
    real(kind=8) :: tolerance, alpha, beta
    real(kind=8), dimension(:,:), allocatable :: v, H
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
        read(ifu,*) natoms
        !
        read(ifu,*)
        read(ifu,*) tolerance
        !
        read(ifu,*)
        read(ifu,*) alpha
        !
        read(ifu,*)
        read(ifu,*) beta
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
        write(unit=uf,fmt='(a)') 'Values of alpha, beta for the Hückel method:'
        write(unit=uf,fmt='(a,f10.5,a)') 'alpha =', alpha, ' eV'
        write(unit=uf,fmt='(a,f10.5,a)') 'beta =', beta, ' eV'
        write(unit=uf, fmt=*)
        !
        write(unit=uf,fmt='(a)') 'Hückel Hamiltonian matrix'
        call write_mat(H, uf, 'f10.5', size(H, dim=2))
        write(unit=uf,fmt=*)
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
        do i = 1, natoms
            write(unit=uf,fmt='(*(f15.5))') ev(i)
        end do
        write(unit=uf,fmt=*)
        !
        write(unit=uf,fmt='(a)') 'Normalised MO coefficient matrix C'
        do i = 1, natoms
            write(unit=uf,fmt='(*(f15.5))') ( V(i,j), j=1, natoms )
        end do
        write(unit=uf,fmt=*)
        !
        return
    end subroutine print_output 

end module io 
