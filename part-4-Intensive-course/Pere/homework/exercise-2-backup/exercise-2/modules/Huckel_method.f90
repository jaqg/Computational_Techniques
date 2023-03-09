! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 23:11:16 08/02/2023 |
! +----------------------------------------------+

module Huckel_method
    implicit none
    contains

    subroutine d_vec(u, v, d)
        !
        ! Subroutine to compute the distance between two vectors, u and v
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: u, v
        real(kind=8), intent(out) :: d
        ! Dummy variables
        integer :: i, n, m 
        real(kind=8) :: suma 
        !
        ! Get size of the vectors
        !
        n = size(u, dim=1)
        m = size(v, dim=1)
        !
        ! Check that they are the same size
        !
        if (n /= m) stop &
        & 'ERROR Huckel_method.f90 d_vec: Different size vectors'
        !
        ! Else, compute the distance
        !
        suma = 0.0_8
        dli: do i = 1, n
            suma = suma + ( v(i) - u(i) )**2
        end do dli
        !
        d = sqrt(suma)
        !
        return
    end subroutine d_vec 

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

    subroutine read_XYZ(filename, natoms, symb, xyz_mat)
        !
        ! Subroutine to read xyz coordinates from file and write to xyz_mat
        ! array
        !
        implicit none
        !
        character(len=*), intent(in) :: filename
        integer, intent(out) :: natoms 
        character(len=2), dimension(:), allocatable, intent(out) :: symb
        real(kind=8), dimension(:,:), allocatable, intent(out) :: xyz_mat
        ! Dummy variables
        integer :: i, nuf, ios, ierr
        !
        ! Open input file
        !
        open(newunit=nuf, file='./data/'//filename, iostat=ios, status="old",action="read")
        if (ios /= 0) stop "read_XYZ: Error opening "//filename
        !
        ! Read number of atoms
        !
        read(nuf,*) natoms
        !
        ! Allocate symb and XYZ matrix
        !
        allocate(symb(natoms), stat=ierr)
        if (ierr .ne. 0) stop 'read_XYZ: Error in allocation of symb'
        !
        allocate(xyz_mat(natoms,3), stat=ierr)
        if (ierr .ne. 0) stop 'read_XYZ: Error in allocation of xyz_mat'
        !
        ! Skip title
        !
        read(nuf,*)
        !
        ! Read XYZ coordinates
        !
        dl1: do i = 1, natoms
            read(nuf,*) symb(i), xyz_mat(i,:)
        end do dl1
        !
        ! Close xyz file
        !
        close(nuf)
        !
        return
    end subroutine read_XYZ 

    subroutine distance_matrix(xyz_mat, dist_mat)
        !
        ! Subroutine to create the distance matrix from the xyz matrix
        !
        implicit none
        !
        real(kind=8), dimension(:,:), intent(in) :: xyz_mat
        real(kind=8), dimension(:,:), allocatable, intent(out) :: dist_mat
        ! Dummy variables
        integer :: i, j, n, ierr
        real(kind=8) :: dist
        !
        n = size(xyz_mat, dim=1)
        !
        allocate(dist_mat(n,n), stat=ierr)
        if (ierr .ne. 0) stop &
        & 'Huckel_method.f90 distance_matrix: Error in allocation of dist_mat'
        !
        ! Main loop
        !
        dl1: do i = 1, n
            !
            ! Diagonal elements
            !
            dist_mat(i,i) = 0.0_8
            !
            ! Non-diagonal elements
            !
            dl2: do j = i+1, n
                !
                ! Calculate the distance between atoms
                !
                call d_vec(xyz_mat(i,:), xyz_mat(j,:), dist)
                !
                ! And store it in the distance matrix. Also, apply symmetry
                !
                dist_mat(i,j) = dist
                dist_mat(j,i) = dist_mat(i,j)
                !
            end do dl2
        end do dl1
        !
        return
    end subroutine distance_matrix

    subroutine topological_matrix(dist_mat, adjency, symbols, topological_mat)
        !
        ! Subroutine to create the topological matrix from the distance matrix
        !
        implicit none
        !
        real(kind=8), dimension(:,:), intent(in) :: dist_mat
        real(kind=8), dimension(2), intent(in) :: adjency 
        character(len=2), dimension(:), intent(in) :: symbols
        real(kind=8),dimension(:,:),allocatable,intent(out) :: topological_mat
        ! Dummy variables
        integer :: i, j, n, nCatoms, ierr
        integer, dimension(:), allocatable :: tmp 
        !
        ! Check that dist_mat and symbols are the same size
        !
        n = size(dist_mat, dim=1)
        !
        if (size(symbols, dim=1) .ne. n) stop &
        &'topological_matrix: Error; dist_mat and symbols are not the same size'
        !
        ! Allocate temporal array
        !
        allocate(tmp(n), stat=ierr)
        if (ierr .ne. 0) stop 'topological_matrix: Error in allocation of tmp'
        !
        ! Count number of C atoms to allocate the topological matrix
        !
        nCatoms = 0
        j = 1
        !
        dl1: do i = 1, n
            if (symbols(i) .eq. 'C ' .or. &
              & symbols(i) .eq. ' C' .or. &
              & symbols(i) .eq. 'c ' .or. &
              & symbols(i) .eq. ' c') then
                !
                ! Track the total number of C atoms
                !
                nCatoms=nCatoms + 1
                !
                ! Store the index of the C atoms in a temporary array
                !
                tmp(j) = i
                j = j + 1
                !
            end if
        end do dl1
        !
        ! Allocate the topological matrix with the number of C atoms
        !
        allocate(topological_mat(nCatoms, nCatoms), stat=ierr)
        if (ierr .ne. 0) stop &
            & 'topological_matrix: Error in allocation of topological_mat'
        !
        ! Create the topological matrix
        !
        dl2: do i = 1, nCatoms
            dl3: do j = 1, nCatoms
                topological_mat(i,j) = dist_mat(tmp(i),tmp(j))
            end do dl3
        end do dl2
        !
        ! Apply the adjency criteria
        !
        dl4: do i = 1, nCatoms
            dl5: do j = 1, nCatoms
                !
                if ( dabs(topological_mat(i,j)) .gt. minval(adjency) .and. &
                    & dabs(topological_mat(i,j)) .lt. maxval(adjency) ) then
                    topological_mat(i,j) = 1.0_8
                else
                    topological_mat(i,j) = 0.0_8
                end if
                !
            end do dl5
        end do dl4
        !
        return
    end subroutine topological_matrix 

    subroutine Huckel_matrix(topolog_mat, alpha, beta, H)
        !
        ! Subroutine to create the Hamiltonian matrix for the Hückel method
        !
        implicit none
        !
        real(kind=8), dimension(:,:), intent(in) :: topolog_mat 
        real(kind=8), intent(in) :: alpha, beta
        real(kind=8), dimension(:,:), allocatable, intent(out) :: H
        ! Dummmy variables
        integer :: i, j, n, ierr
        !
        ! For the Hückel approximation
        !
        ! S_{ij} = delta_{ij}
        !
        ! the Hamiltonian matrix, H, reads
        !             -
        !            | alpha    if i = j
        ! H_{i,j} = -  beta     if i,j adjacent
        !            | 0        otherwise
        !             -
        !
        ! Allocate needed arrays
        !
        n = size(topolog_mat, dim=1)
        !
        allocate(H(n,n), stat=ierr)
        if (ierr .ne. 0) stop &
            & 'ERROR Huckel_method.f90 huckel_matrix: Error in allocation of H'
        !
        ! Initialize H
        !
        H = 0.0_8
        !
        ! Compute H
        !
        dli: do i = 1, n
            !
            ! Diagonal elements
            !
            H(i,i) = alpha
            !
            dlii: do j = i+1, n
                !
                ! Non-diagonal elements
                !
                H(i,j) = beta * topolog_mat(i,j)
                !
                ! Construct the rest of the matrix from symmetry
                !
                H(j,i) = H(i,j)
                !
            end do dlii
        end do dli
        !
        return
    end subroutine Huckel_matrix 
        
    subroutine sort_evals_evec(ev, v)
        !
        ! Subroutine to sort the eigenvalues, ev, and eigenvectors, v, in 
        ! ascending order
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(inout) :: ev
        real(kind=8), dimension(:,:), intent(inout) :: v
        ! Dummy variables
        integer :: i, j, n, ierr 
        real(kind=8) :: tmp 
        real(kind=8), dimension(:), allocatable :: tmp_arr
        !
        ! Check that they match sizes
        !
        n = size(ev, dim=1)
        !
        if (size(v, dim=2) .ne. n) stop &
            & 'sort_evals_evec: ev and v do not match size.' 
        !
        ! Allocate needed arrays
        !
        allocate(tmp_arr(n), stat=ierr)
        if (ierr .ne. 0) stop &
        & 'Huckel_method.f90 sort_evals_evec: Error in allocation of tmp_arr'
        !
        ! Initialize temporal variables
        !
        tmp = 0.0_8
        tmp_arr = 0.0_8
        !
        ! Main loop
        !
        dli: do i = 1, n-1
            dlii: do j = i + 1, n
                if (ev(i) > ev(j)) then 
                    !
                    ! Swap the larger eigenvalue with the smaller one
                    !
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
                    ! and v(:,j) the one corresponding to the larger eigenval
                    v(:,j) = tmp_arr(:)
                    !
                end if
            end do dlii
        end do dli
        !
        return
    end subroutine sort_evals_evec 

    subroutine Huckel_evals_evecs(ev, v)
        implicit none
        real(kind=8), dimension(:), intent(inout) :: ev
        real(kind=8), dimension(:,:), intent(inout) :: v
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
