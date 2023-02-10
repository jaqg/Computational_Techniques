! =========================================================================================
!             Huckel Method for Pi Orbitals in Molecules (Linear Algebra Methods)
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

module LAmethods

    implicit none

    public :: HuckMat, Jacobi, Mulliken, Occupancy, QuickSort, ShowMat

    contains

    real(kind=8) function distance(vec1, vec2, d)
    ! -------------------------------------------------------------------------------------
    ! Function to compute the distance between two n-D vectors
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - vec1: array with the x, y and z coordinates of vector 1
    ! - vec2: array with the x, y and z coordinates of vector 2
    ! - d:   size of the vectors
    !
    ! Outputs
    ! - distance: real number for the euclidian distance between the two vectors
    !
    ! -------------------------------------------------------------------------------------

        ! Defining variables --------------------------------------------------------------
        integer :: d, i
        real(kind=8), dimension(d) :: vec1, vec2, vec3
        real(kind=8) :: temp
        vec3 = vec1 - vec2             ! Compute new vector (subtraction of old vectors)
        temp = 0
        do i = 1, d
            temp = temp + vec3(i)**2   ! Calculate the sum of the square of each coordinate
        end do
        distance = sqrt(temp)          ! Calculate the square root of the previous sum
    end function

    real(kind=8) function convErr(mat, d)
    ! -------------------------------------------------------------------------------------
    ! Function to detect if the matrix is diagonal
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - mat: matrix of which should be diagonal
    ! - d:   size of the smallest side of the matrix
    !
    ! Outputs
    ! - convErr: the value of any non-diagonal element which is not 0
    !
    ! -------------------------------------------------------------------------------------

        ! Defining variables --------------------------------------------------------------
        integer :: d, i, j
        real(kind=8), dimension(d,d) :: mat
        real(kind=8) :: temp
        
        do i = 1, d
            do j = 1, d
                temp = abs(mat(i,j))
                if ((i /= j) .and. (temp > 1D-9)) then
                    convErr = temp
                    return
                end if
            end do
        end do
    end function

    real(kind=8) function theta(mat, pos, d)
    ! -------------------------------------------------------------------------------------
    ! Function to compute the angle theta for the Jacobi method
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - mat: matrix which will later be rotated
    ! - pos: the position of the maximum value in the matrix
    ! - d:   size of the smallest side of the matrix
    !
    ! Outputs
    ! - theta: real number for the angle to use to compute the orthogonal matrix
    !
    ! -------------------------------------------------------------------------------------

        ! Defining variables --------------------------------------------------------------
        integer :: d
        real(kind=8), dimension(d,d) :: mat
        integer, dimension(2) :: pos
        real(kind=8) :: PI

        PI = 3.141592653589                                ! Defining PI on foot

        if (mat(pos(1),pos(1)) == mat(pos(2),pos(2))) then ! Case where a[i,i] = a[j,j]
            theta = PI/4                                   ! This is just pi/4
        else
            theta = 0.5 * datan(2*mat(pos(1),pos(2)) / (mat(pos(2),pos(2)) - mat(pos(1),pos(1))))
        end if
    end function

    subroutine DistMat(xyz, dMat)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to compute the distances between the atoms in the molecule
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - xyz:  array with XYZ coordinates of the atoms in the molecule
    !
    ! Outputs
    ! - dMat: matrix with the interatomic distances between every pair of atoms
    !
    ! Observation:
    !   I was going to use this subroutine as suggested in the homework. However, I noticed
    !   that this method is inefficient for the task at hand. Computing the distance for
    !   every pair, and then having to remove hydrogens from such a big matrix isn't good.
    !   Better to remove the hydrogens from the XYZ coordinates and then compute the
    !   distances only for the other atoms. Finally, since the Huckel matrix is symmetric,
    !   only half of the distances have to be computed.
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        real(kind=8), allocatable, intent(in) :: xyz(:,:)
        real(kind=8), allocatable, intent(out) :: dMat(:,:)

        ! Internal variables
        integer :: i, j, n

        ! Check how many atoms are there
        n = size(xyz, 1)

        ! Assign memory to the distance matrix
        allocate(dMat(n,n))

        do i = 1, n
            do j = 1, n
                dMat(i,j) = distance(xyz(i,:), xyz(j,:), 3)
            end do
        end do

    end subroutine DistMat

    subroutine HuckMat(xyz, huck)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to create a Huckel matrix from the coordinates of the atoms in
    ! a molecule
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - xyz:  array with XYZ coordinates of the atoms in the molecule
    !
    ! Outputs
    ! - huck: a square Huckel matrix with 1s where pi bonds are detected and 0 otherwise
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        real(kind=8), allocatable, intent(in) :: xyz(:,:)
        real(kind=8), allocatable, intent(out) :: huck(:,:)

        ! Internal variables
        real(kind=8) :: dist, critical
        integer :: i, j, n

        ! Setting the maximum distance for a double bond
        critical = 1.45

        ! Check how many atoms are there
        n = size(xyz, 1)

        ! Assign memory to the Huckel matrix
        allocate(huck(n,n))

        ! Initializing the Huckel matrix
        huck = 0

        do i = 1, n                           ! Go atom by atom      - atom1
            do j = i, n                       ! Also go atom by atom - atom2
                if (i == j) then              ! If atom1 = atom2 ...
                    huck(i,j) = 0             ! Set a 0 in the i,j position of the matrix
                else                          ! Otherwise ... compute the distance, ...
                    dist = distance(xyz(i,:), xyz(j,:), 3)
                    if (dist < critical) then ! ... check if the distance is small enough
                        huck(i,j) = 1         ! If so, set 1 in the i,j position
                        huck(j,i) = 1         ! By symmetry, set 1 in the j,i position
                    else                      ! Otherwise ...
                        huck(i,j) = 0         ! ... set a 0 in the i,j position
                        huck(j,i) = 0         ! By symmetry, set 0 in the j,i position
                    end if
                end if
            end do
        end do

    end subroutine HuckMat

    subroutine Jacobi(huck, eigVal, eigVec)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to compute the eigenvalues and eigenvectors of a square symmetric
    ! matrix using the Jacobi algorithm
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - huck: the topological Huckel matrix for the pi-conjugated atoms in a molecule
    !
    ! Outputs
    ! - eigVal: a list of real numbers, the eigenvalues of the matrix
    ! - eigVec: a matrix of real numbers, whose columns are the eigenvectors of the matrix
    !
    ! -------------------------------------------------------------------------------------
        
        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        real(kind=8), allocatable, intent(in) :: huck(:,:)
        real(kind=8), allocatable, intent(out) :: eigVal(:), eigVec(:,:)

        ! Internal variables
        real(kind=8), allocatable :: oRota(:,:), thuck(:,:), identity(:,:)
        integer, dimension(2) :: maxPos
        logical, allocatable :: hidden(:,:)
        real(kind=8) :: angle
        integer :: d, i, j

        ! Get the size of the matrix
        d = size(huck, 1)

        ! Assign memory to all the matrices to be used
        allocate(thuck(d,d))    ! Temporary Huckel (to be used instead of Huckel)
        allocate(eigVec(d,d))   ! Eigenvector matrix
        allocate(hidden(d,d))   ! Mask to consider only upper-triangular values
        allocate(identity(d,d)) ! Identity matrix (to avoid computing it every time)
        allocate(oRota(d,d))    ! The orthogonal rotation matrix
        allocate(eigVal(d))     ! The list of eigenvalues

        ! Initialize the mask and the identity matrices
        hidden = .true.
        identity = 0

        do i = 1, d
            identity(i,i) = 1
            do j = 1, i
                hidden(i,j) = .false.
            end do
        end do

        ! Initialize the temporary Huckel, and eigenvector matrices
        thuck = huck
        eigVec = identity

        ! Jacobi algorithm
        do while (convErr(thuck, d) > 1D-9)          ! If error of any entry over threshold
            maxPos = maxloc(abs(thuck), hidden)      ! Find the location of highest value
            angle = theta(thuck, maxPos, d)          ! Compute the rotation angle
            oRota = identity                         ! Initialize orthogonal rotation ...
            oRota(maxPos(1),maxPos(2)) = dsin(angle) ! ... matrix based on identity and ...
            oRota(maxPos(2),maxPos(1)) = -dsin(angle)! ... the 4 trigonometric functions.
            oRota(maxPos(1),maxPos(1)) = dcos(angle)
            oRota(maxPos(2),maxPos(2)) = dcos(angle)

            ! Compute the rotation transformations over the temporary Huckel matrix
            thuck = matmul(transpose(oRota),matmul(thuck, oRota))
            ! Compute the product of the eigenvector- and the orthogonal rotation matrix
            eigVec = matmul(eigVec, oRota)
        end do

        do i = 1, d
            ! Fill the eigenvalue list with the diagonal values of the eigenvalue matrix.
            eigVal(i) = thuck(i,i)
            ! Normalize each eigenvector
            eigVec(:,i) = eigVec(:,i) * 1/sqrt(dot_product(eigVec(:,i),eigVec(:,i)))
        end do

    end subroutine Jacobi

    subroutine Mulliken(eValS, eVec, occ, bondOrder, mulChrg)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to compute the bond order and Mulliken charge
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - eValS:  the matrix of sorted eigenvalues and their index
    ! - eVec:   the matrix of eigenvectors
    ! - occ:    a list of the occupancies of the orbitals
    !
    ! Outputs
    ! - bondOrder:  a real number matrix with the bond order of each pair of atoms
    ! - mulChrg:    a list of real numbers with the Mulliken charge of each atom
    !
    ! -------------------------------------------------------------------------------------

        implicit none
        
        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        real(kind=8), allocatable, intent(in) :: eValS(:,:), eVec(:,:), occ(:)
        real(kind=8), allocatable, intent(out) :: bondOrder(:,:), mulChrg(:)

        ! Internal variables
        real(kind=8) :: temp
        integer :: d, i, j, k, index

        ! Get the number of eigenstates involved in the pi system
        d = size(eValS, 1)

        allocate(mulChrg(d))
        allocate(bondOrder(d,d))

        do i = 1, d                                       ! Go through every atom
            do j = 1, d                                   ! Go through every atom
                temp = 0
                do k = 1, d
                    index = int(eValS(d-k+1,2))           ! Get the atom's index

                    ! Compute density times occupancy
                    temp = temp + occ(k) * eVec(i,index) * eVec(j,index)
                end do
                bondOrder(i,j) = temp                     ! Save value of bond order
            end do
        end do

        mulChrg = (/ (bondOrder(i,i), i = 1, d) /)        ! Extracting the Mulliken charge

    end subroutine Mulliken

    subroutine Occupancy(eValS, charge, occ)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to compute the occupancy of each orbital
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - eValS:  the matrix of sorted eigenvalues and their index
    ! - charge: the charge of the molecule
    !
    ! Outputs
    ! - occ:    a list of real numbers with the occupancy of each orbital
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        real(kind=8), intent(in) :: charge
        real(kind=8), allocatable, intent(in) :: eValS(:,:)
        real(kind=8), allocatable, intent(out) :: occ(:)

        ! Internal variables
        real(kind=8), allocatable :: ieVals(:)
        integer, allocatable :: degenerate(:,:)
        real(kind=8) :: nElec, nUnpElec, nDegenOrbs, nDegenElec
        integer :: d, i, j, ctrl, nFullMO, pivot

        ! Get the number of atoms involved in the pi system
        d = size(eValS, 1)

        nElec = d - charge             ! Determine the number of electrons in the py system
        nFullMO = floor(nElec / 2)     ! Compute the number of full Orbs (Aufbau + Pauli)
        nUnpElec = mod(nElec, 2.0)     ! Are there any unpaired electrons?

        allocate(occ(d))               ! Assign memory to occupancy list
        allocate(degenerate(d,2))
        allocate(ieVals(d))

        ieVals = eValS(d:1:-1,1)       ! Reversing the eigenvalue list
        degenerate = 0                 ! Initializing degeneracy control
        occ = 0                        ! Initialize occupancy list

        ! Check which eigenvalues are degenerate ...
        do i = 1, d
            ! ... and group them by first appearence
            degenerate(i,1) = i
            if ( i /= 1 ) then
                if (abs(ieVals(i-1) - ieVals(i)) < 1D-8) then
                    degenerate(i,1) = degenerate(i-1,1)
                end if
            end if
            ! ... and group them by last appearence
            j = d - i + 1
            degenerate(j,2) = j
            if ( j /= d ) then
                if (abs(ieVals(j+1) - ieVals(j)) < 1D-8) then
                    degenerate(j,2) = degenerate(j+1,2)
                end if
            end if
        end do

        occ(1:nFullMO) = 2             ! Fill all pairs of electrons in the Orbs

        pivot = nFullMO                ! Position of frontier orbital

        if (nUnpElec /= 0) then        ! If there are unpaired electrons ...
            write(*, *) '--------------------------------------------------------------------------'
            write(*, *) "|         WARNING: The molecule contains an unpaired electron.           |"
            write(*, *) '--------------------------------------------------------------------------'
            occ(nFullMO+1) = 1
            pivot = nFullMO + 1
        end if

        ! If there is degeneracy in the last filled level
        if (degenerate(pivot, 1) /= degenerate(pivot, 2)) then

            ! Count the number of electrons in that degenerate level
            nDegenElec = sum(occ(degenerate(pivot, 1):degenerate(pivot, 2)))
            
            ! Count the number of degenerate orbitals
            nDegenOrbs = (degenerate(pivot, 2) - degenerate(pivot, 1) + 1)
            
            ! Fill in the degenerate orbitals as fractionary occupations
            occ(degenerate(pivot, 1):degenerate(pivot, 2)) = nDegenElec / real(nDegenOrbs,8)
        end if

    end subroutine Occupancy

    subroutine ShowMat(M)
    ! -------------------------------------------------------------------------------------
    ! Helper subroutine to help me see the contents of any matrix when testing
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - M:  Matrix whose values I will see
    !
    ! Outputs
    ! - on screen: the matrix in its natural representation
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        real(kind=8), allocatable, intent(in) :: M(:,:)

        ! Internal variables
        integer :: i, j, f, c

        ! Get the number of columns and rows
        c = size(M, 1)
        f = size(M, 2)

        write(*, *) '--------------------------------------------------------------------------'
        do i=1, f
            do j=1, c
                write(*, "(' ', F12.8)", advance='no') M(j,i)
            end do
            write(*, *) ""
        end do
        write(*, *) '--------------------------------------------------------------------------'

    end subroutine ShowMat

    recursive subroutine QuickSort(mat)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to sort the elements of a matrix by the value in its first
    ! dimension [RECURSIVE!] - it uses the QuickSort algorithm to make it efficient
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - mat: the matrix of values to be sorted
    !
    ! Outputs
    ! - mat: the matrix with its values of the first dimension sorted
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        real(kind=8), allocatable, intent(inout) :: mat(:,:)

        ! Internal variables
        real(kind=8), allocatable :: aleft(:,:), aright(:,:), temp(:)
        integer :: i, d1, d2, p1, p2

        ! Get the dimensions of the matrix
        d1 = size(mat, 1)
        d2 = size(mat, 2)

        ! Assign memory to a temporal vector to hold data on the fly
        allocate(temp(d2))

        if (d1 > 1) then ! If the matrix has only one element in its first dimension, leave
            p1 = 1                             ! Set moving pivot position
            p2 = d1                            ! Set static pivot position

            do i = 1, d1                       ! Move through matrix ...
                if (mat(i,1) < mat(p2,1)) then ! If i-th value is less than static pivot ..
                    temp = mat(p1,:)           ! ... save data from moving pivot ...
                    mat(p1,:) = mat(i,:)       ! ... move data from i-th to moving pivot ..
                    mat(i,:) = temp            ! ... set saved data to i-th pivot
                    p1 = p1 + 1                ! Increment moving pivot
                end if
            end do

            ! When finished separating the data in lesser and greater than static pivot ...
            temp = mat(p1,:)                   ! ... save data from moving pivot ...
            mat(p1,:) = mat(p2,:)              ! ... move data from static to moving pivot
            mat(p2,:) = temp                   ! ... set saved data to static pivot

            ! Assign memory to temporary segment of matrices
            allocate(aleft(p1-1,2))
            allocate(aright(d1-p1,2))

            ! Save segment of matrices to sort them individually
            aleft = mat(1:p1-1,:)

            ! Call **this same subroutine** to sort each segment
            call QuickSort(aleft)

            ! Save the sorted segments to the original matrix
            mat(1:p1-1,:) = aleft

            ! If the moving pivot is not equal to the first dimension, repeat the same
            if (p1 < d1) then
                aright = mat(p1+1:d1,:)
                call QuickSort(aright)
                mat(p1+1:d1,:) = aright
            end if

        end if

    end subroutine QuickSort

end module LAmethods