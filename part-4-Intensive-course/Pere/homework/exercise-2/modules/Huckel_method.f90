! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 23:11:16 08/02/2023 |
! +----------------------------------------------+

module Huckel_method
    implicit none
    contains

    function norm_vec(v) result(v_norm)
        !
        ! Function that normalizes to one the vector v (real)
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: v
        real(kind=8), dimension(:), allocatable :: v_norm
        integer :: i, n, ierr
        real(kind=8) :: suma, magn_vec
        !
        n = size(v,1)
        !
        suma = 0.0_8
        do i = 1, n
            suma = suma + v(i)**2
        end do
        !
        magn_vec = dsqrt(suma)
        !
        allocate(v_norm(n), stat=ierr)
        if (ierr .ne. 0) stop 'ERROR jacobi_method.f90 func norm_vec: &
            & Error in allocation of v_norm'
        !
        v_norm = v/magn_vec
        !
        return
    end function norm_vec

    subroutine sort_evals_evec(ev, v)
        implicit none
        real(kind=8), dimension(:), intent(inout) :: ev
        real(kind=8), dimension(:,:), intent(inout) :: v
        !
        integer :: i, j, n, ierr 
        real(kind=8) :: tmp 
        real(kind=8), dimension(:), allocatable :: tmp_arr
        !
        n = size(ev, dim=1)
        !
        allocate(tmp_arr(n), stat=ierr)
        if (ierr .ne. 0) stop &
        & 'Huckel_method.f90 sort_evals_evec: Error in allocation of tmp_arr'
        !
        tmp = 0.0_8
        tmp_arr = 0.0_8
        !
        dli: do i = 1, n-1
            dlii: do j = i + 1, n
                if (ev(i) > ev(j)) then 
                    ! Store the value in a temporal variable
                    tmp = ev(i)
                    ! Let ev(i) be the smaller number
                    ev(i) = ev(j)
                    ! and ev(j) the larger number
                    ev(j) = tmp
                    !
                    ! Swap the eigenvectors (columns of V) to match the
                    ! sorted eigenvalues
                    !
                    ! Store the vector in a temporal array
                    tmp_arr(:) = v(:,i)
                    ! Let v(:,i) be the vector corresponding the smaller eigenv
                    v(:,i) = v(:,j)
                    ! and v(:,j) the one corresponding the larger eigenval
                    v(:,j) = tmp_arr(:)
                    !
                end if
            end do dlii
        end do dli
        !
        return
    end subroutine sort_evals_evec 

    subroutine Huckel_matrix(n, alpha, beta, H)
        implicit none
        integer, intent(in) :: n
        real(kind=8), intent(in) :: alpha, beta 
        real(kind=8), dimension(:,:), allocatable, intent(out) :: H
        !
        integer :: i, j, ierr
        !
        ! For the Hückel approximation
        !
        ! S_{ij} = delta_{ij}
        !
        ! the Hamiltonian matrix H reads
        !             -
        !            | alpha    if i = j
        ! H_{i,j} = -  beta     if i,j adjacent
        !            | 0        otherwise
        !             -
        allocate(H(n,n), stat=ierr)
        if (ierr .ne. 0) stop &
            & 'ERROR Huckel_method.f90 huckel_matrix: Error in allocation of H'
        ! Initialize H
        H = 0.0_8
        dli: do i = 1, n
            dlii: do j = i, n
                if (i == j) H(i,j) = alpha
                if (abs(i-j) == 1) H(i,j) = beta
                ! Construct the rest of the matrix from symmetry
                H(j,i) = H(i,j)
            end do dlii
        end do dli
        !
        return
    end subroutine Huckel_matrix 
        
    subroutine Huckel_evals_evecs(ev, v)
        implicit none
        real(kind=8), dimension(:), intent(out) :: ev
        real(kind=8), dimension(:,:), intent(out) :: v
        !
        integer :: i 
        !
        ! Sort the eigenvalues in ascending order, and so the eigenvectors
        !
        call sort_evals_evec(ev, v)
        !
        ! Normalise eigenvectors
        !
        do i = 1, size(v, dim=2)
            v(:,i) = norm_vec(v(:,i))
        end do
        !
        return
    end subroutine Huckel_evals_evecs 
end module Huckel_method
