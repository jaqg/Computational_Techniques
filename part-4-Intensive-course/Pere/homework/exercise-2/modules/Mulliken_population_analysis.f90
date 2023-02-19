! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Thursday 22:17:03 09/02/2023 |
! +---------------------------------------------+
!
! module Mulliken_pop_analysis
!     implicit none
!     contains
!
!     subroutine Mulliken_population_matrix(nMO, C, S, P)
!         !
!         ! nMO: number of molecular orbitals
!         ! C: coefficient matrix of the AO's for each MO (columns)
!         ! S: overlap matrix between AO's
!         ! P: Mulliken population matrix
!         !
!         implicit none
!         integer, intent(in) :: nMO 
!         real(kind=8), dimension(:,:), intent(in) :: C, S
!         real(kind=8), dimension(:,:), allocatable, intent(out) :: P
!         !
!         
!         !
!         return
!     end subroutine Mulliken_population_matrix 
!         
! end module Mulliken_pop_analysis
