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
    integer :: i, j, uf, ifu, ofu, ios, istat, n, totiter, natoms, nCatoms, &
    & charge, npielec
    character(len=80) :: xyzfile, outputfn 
    character(len=2), dimension(:), allocatable :: symbols
    real(kind=8) :: tolerance, alpha, beta, EHuckel
    real(kind=8), dimension(2) :: adjency 
    real(kind=8), dimension(:,:), allocatable :: v, H, H_diag, xyz, dist_mat, &
    & topological_mat, pi_BO
    real(kind=8), dimension(:), allocatable :: ev, n_occ, n_occ_pi
    real(kind=8), dimension(3,3) :: A 

    contains

    subroutine read_input
        !
        open(newunit=ifu, file='./data/input.dat', iostat=ios, status="old", &
        & action="read")
        if (ios /= 0) stop "Error opening file 'data/input.dat'"
        !
        read(ifu,*)
        read(ifu,*) xyzfile
        xyzfile = trim(xyzfile)
        !
        read(ifu,*)
        read(ifu,*) charge
        !
        read(ifu,*)
        read(ifu,*) adjency(:)
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
        write(uf,'(*(a))') 'xyz file: ', xyzfile
        write(uf,'(a,i0)') 'Charge of the molecule: ', charge
        write(uf,*)
        !
        write(unit=uf,fmt='(a)') 'Values of alpha, beta for the Hückel method:'
        write(unit=uf,fmt='(a,f10.5,a)') 'alpha =', alpha, ' eV'
        write(unit=uf,fmt='(a,f10.5,a)') 'beta =', beta, ' eV'
        write(unit=uf, fmt=*)
        !
        write(uf,*) 'Distance matrix:'
        call write_mat(dist_mat, uf, 'f8.4', size(dist_mat, dim=2))
        write(uf,*)
        !
        write(uf,*) 'Topological matrix:'
        call write_mat(topological_mat, uf, 'f10.4', size(topological_mat, dim=2))
        write(uf,*)
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
        do i = 1, nCatoms
            write(unit=uf,fmt='(*(f15.5))') ev(i)
        end do
        write(unit=uf,fmt=*)
        !
        write(unit=uf,fmt='(a)') 'Normalised MO coefficient matrix C'
        do i = 1, nCatoms
            write(unit=uf,fmt='(*(f15.5))') ( V(i,j), j=1, nCatoms )
        end do
        write(unit=uf,fmt=*)
        !
        write(unit=uf,fmt='(a)') '--- Mulliken population analysis ---'
        write(unit=uf, fmt=*)
        !
        write(unit=uf,fmt='("Number of pi electrons: ", i0)') npielec
        write(unit=uf, fmt=*)
        !
        write(unit=uf,fmt='("Hückel energy: ", f10.4, " eV")') EHuckel
        write(unit=uf, fmt=*)
        !
        write(unit=uf,fmt='(a)') 'Occupation numbers:'
        write(unit=uf,fmt='(*(f10.4))') n_occ
        write(unit=uf, fmt=*)
        !
        write(unit=uf,fmt='(a)') 'Pi electron population:'
        write(unit=uf,fmt='(*(f10.4))') n_occ_pi
        write(unit=uf, fmt=*)
        !
        write(unit=uf,fmt='(a)') 'Pi-bond order:'
        call write_mat(pi_BO, uf, 'f10.4', size(pi_BO, dim=2))
        write(unit=uf, fmt=*)
        !
        return
    end subroutine print_output 

end module io 
