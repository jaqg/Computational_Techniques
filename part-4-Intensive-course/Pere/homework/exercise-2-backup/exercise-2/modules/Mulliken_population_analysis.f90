! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Thursday 22:17:03 09/02/2023 |
! +---------------------------------------------+

module Mulliken_pop_analysis
    implicit none
    contains

    subroutine Mulliken_population(charge, nC, evals, coefs, &
        & npielec, EHuckel, n_occ, n_occ_pi, pi_BO)
        !
        ! Subroutine to perform the Mulliken populations analysis
        !   - Number of pi electrons
        !   - Hückel energy
        !   - Occupation numbers
        !   - Pi electron population
        !   - Pi-bond order
        !
        implicit none
        !
        integer, intent(in) :: charge, nC
        real(kind=8), dimension(:), intent(in) :: evals
        real(kind=8), dimension(:,:), intent(in) :: coefs
        integer, intent(out) :: npielec 
        real(kind=8), intent(out) :: EHuckel 
        real(kind=8), dimension(:), allocatable, intent(out) :: n_occ, n_occ_pi
        real(kind=8), dimension(:,:), allocatable, intent(out) :: pi_BO
        ! Dummy variables
        integer :: i, j, k, ierr
        real(kind=8) :: tmp
        !
        ! Check that the size of the eigenvalues vector and coefficients 
        ! array is the same
        !
        if (size(evals, dim=1) .ne. size(coefs, dim=2)) stop &
        & 'ERROR Mulliken_population: evals and coefs are not the same size'
        !
        ! Number of pi electrons
        !
        npielec = nC - charge
        write(*,*) 'npielec =', npielec
        !
        ! Occupation numbers
        !
        allocate(n_occ(size(coefs, dim=2)), stat=ierr)
        if (ierr .ne. 0) stop &
            & 'Mulliken_population: Error in allocation of n_occ'
        !
        n_occ = 0
        dol1: do i = 1, npielec/2
            n_occ(i) = 2
        end do dol1
        !
        if (nint(real(npielec/2)).ne.npielec/2) n_occ(nint(real(npielec/2)))=1
        !
        ! Hückel energy
        !
        EHuckel = 0.0_8
        dhl1: do i = 1, size(evals, dim=1)
            EHuckel = EHuckel + dble(n_occ(i)) * evals(i)
        end do dhl1
        !
        ! pi-electron population
        !
        allocate(n_occ_pi(size(coefs, dim=2)), stat=ierr)
        if (ierr .ne. 0) stop &
            & 'Mulliken_population: Error in allocation of n_occ_pi'
        !
        n_occ_pi = 0.0_8
        dpl1: do j = 1, size(coefs, dim=1)
            tmp = 0.0_8
            dpl2: do i = 1, size(coefs, dim=2)
                tmp = tmp + dble(n_occ(i)) * dabs(coefs(j,i))**2
            end do dpl2
            n_occ_pi(j) = tmp
        end do dpl1
        !
        ! pi-bond order
        !
        allocate(pi_BO(size(coefs, dim=1), size(coefs, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop &
            & 'Mulliken_population: Error in allocation of pi_BO'
        !
        dbol1: do j = 1, size(coefs, dim=1)
            dbol2: do k = 1, size(coefs, dim=1)
                tmp = 0.0_8
                dbol3: do i = 1, size(coefs, dim=2)
                    tmp = tmp + dble(n_occ(i)) * coefs(j,i) * coefs(k,i)
                end do dbol3
                pi_BO(j,k) = tmp
            end do dbol2
        end do dbol1
        !
        return
    end subroutine Mulliken_population 
        
end module Mulliken_pop_analysis
