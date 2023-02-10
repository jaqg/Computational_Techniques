! =========================================================================================
!                 Huckel Method for Pi Orbitals in Molecules (Input Output Methods)
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

module IOmethods

    implicit none

    public :: ReadXYZ, removeHs, ShowResults, WriteHuckel, WriteMulliken

    contains

    subroutine ReadXYZ(xyz_fil_nam, atom_nam, XYZ_coords)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to read and XYZ file
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - xyz_fil_nam: name of the file containing the XYZ data
    !
    ! Outputs
    ! - XYZ_coords: an array with the XYZ coordinates of a molecule
    !
    ! -------------------------------------------------------------------------------------

        implicit none
        
        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        character(*), intent(in) :: xyz_fil_nam
        character(len=2), allocatable, intent(out) :: atom_nam(:)
        real(kind=8), allocatable, intent(out) :: XYZ_coords(:,:)
        
        ! Internal variables
        character(len=1000) :: leer, title
        integer :: i, j, filas, cols
        integer, dimension(3) :: position_check
        
        ! Open the file
        open(40, file=xyz_fil_nam, status='old')
        
        ! Read the first line of the file as text
        read(40, '(a)') leer

        ! Read the title of the file
        read(40, '(a)') title

        ! Parse the first number in the XYZ file as an integer
        read(leer, *) filas

        ! Assign memory to the atoms and XYZ arrays
        allocate(atom_nam(filas))
        allocate(XYZ_coords(filas,3))

        ! Initializing XYZ coordinates
        XYZ_coords = 0
        
        do i = 1, filas                        ! For every line declared in the header ...
            read(40, '(a)') leer               ! ... read each line as a string
            position_check = 0                 ! Set the slicing positions to 0
            leer = adjustl(leer)               ! Remove leading spaces from line               
            atom_nam(i) = leer(1:2)            ! Extract first 2 characters as atom symbol
            position_check(1) = 3              ! Set first slicing position to 3
            do j = 4, 100                      ! For every character in the rest of line

                ! Check if the next character is blank, but this one isn't ....
                if ((leer(j+1:j+1) == ' ') .and. (leer(j:j) /= ' ')) then
                    ! If this isn't the first case any more, but the second is available ..
                    if ((position_check(1) /= 0) .and. (position_check(2) == 0)) then
                        position_check(2) = j + 1          ! Set second slicing position
                    else if (position_check(3) == 0) then
                        position_check(3) = j + 1          ! Set third slicing position
                        exit
                    else
                        write(*, *) "An error has occured!"! Something has gone terribly
                        write(*, *) "Check your XYZ file." ! wrong.
                        stop
                    end if
                end if
            end do

            ! Save the X, Y and Z coordinates to the matrix, according to the slicing
            ! positions
            read(leer(position_check(1):position_check(2)), *) XYZ_coords(i,1)
            read(leer(position_check(2):position_check(3)), *) XYZ_coords(i,2)
            read(leer(position_check(3):100), *) XYZ_coords(i,3)
        end do

        ! Close the file
        close(40)

    end subroutine ReadXYZ

    subroutine removeHs(symb, xyz, newSymb, newXYZ)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to remove the H atoms from XYZ data
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - symb: array with the symbols of all the atoms in the molecule
    ! - xyz:  array with XYZ coordinates of the atoms in the molecule
    !
    ! Outputs
    ! - newSymb: array with the symbols of all the atoms in the molecule minus hydrogen
    ! - newXYZ:  array with XYZ coordinates of the atoms in the molecule minus hydrogen
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        character(len=2), allocatable, intent(in) :: symb(:)
        real(kind=8), allocatable, intent(in) :: xyz(:,:)
        character(len=2), allocatable, intent(out) :: newSymb(:)
        real(kind=8), allocatable, intent(out) :: newXYZ(:,:)

        ! Internal variables
        integer :: n, n2, i, j, nhs

        ! Get the total number of atoms
        n = size(symb)

        ! Count the number of non-Hydrogen atoms in the molecule
        nhs = 0
        do i = 1, n
            if (trim(symb(i)) /= "H") then
                nhs = nhs + 1
            end if
        end do

        ! Assign memory to the new atoms and coordinates arrays
        allocate(newSymb(nhs))
        allocate(newXYZ(nhs,3))

        ! Initialize position of the new arrays
        n2 = 1
        do i = 1, n                         ! While going atom by atom of the old arrays ..
            if (trim(symb(i)) /= "H") then  ! ... check if the atom is not a H
                newSymb(n2) = symb(i)       ! And if so, add this atom to the new arrays
                newXYZ(n2,:) = xyz(i,:)
                n2 = n2 + 1                 ! Move the position in the arrays
            end if
        end do

    end subroutine removeHs

    subroutine ShowResults(eVal, eVec, occ, a, b, d, bondOrd)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to show all the results from the Huckel and Mulliken analysis
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - eValS:      the matrix of sorted eigenvalues and their index
    ! - eVec:       the matrix of eigenvectors
    ! - occ:        a list of the occupancies of the orbitals
    ! - a:          alpha parameter for the energy
    ! - b:          beta parameter for the energy
    ! - d:          how many atoms are involved in the pi-system
    ! - bondOrder:  a real number matrix with the bond order of each pair of atoms
    !
    ! Outputs
    ! - on screen:  The number of eigenstate, accompanied by the eigenvalue, the energy
    !               value, the occupancy, and a table with the eigenvectors.
    !
    !               Also, the Mulliken charge, and the bond order matrix for all pairs of
    !               atoms.
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        real(kind=8), allocatable, intent(in) :: eVal(:,:), eVec(:,:), bondOrd(:,:), occ(:)
        real(kind=8), intent(in) :: a, b
        integer, intent(in) :: d

        ! Internal variables
        character(len=60) :: ending
        real(kind=8) :: energy, columnas
        integer :: i, j, k, filas, index, endPivot, residual

        energy = 0
        do i = 1, d
            if (occ(i) /= 0) then
                energy = energy + occ(i) * (a + eVal(d-i+1,1) * b)
            end if
        end do

        write(*, *) ""
        write(*, *) ""

        write(*, *) '--------------------------------------------------------------------------'
        write(*, "(' ', A, F16.8, A)") '|  Final π-orbital energy = ', energy, '                             |'
        write(*, *) '--------------------------------------------------------------------------'

        ! Finding out how many rows I will have to generate
        residual = int(mod(d, 6))
        filas = floor(1.0 * d / 6)
        if (residual /= 0) then
            filas = filas + 1
        end if

        ! Showing all the results from the Huckel analysis in a pretty way ----------------
        write(*, *) '--------------------------------------------------------------------------'
        write(*, *) '|                              Eigenstates                               |'

        do i = 1, filas

            endPivot = 2

            ending = " |"

            if ((residual /= 0) .and. (i == filas)) then         ! Formatting rows and columns
                columnas = real(d,8)/filas
                do j = 1, (6 - residual)
                    ending = "          "//ending
                    endPivot = endPivot + 10
                end do
            else
                columnas = 6
            end if

            write(*, *) '--------------------------------------------------------------------------'

            write(*, "(' ', A)", advance='no') "|Eigenstat|"
            do j = (i-1)*6+1, ceiling(i*columnas)
                write(*, "(' ', I9)", advance='no') j
            end do
            write(*, *) ending(1:endPivot)

            write(*, *) '--------------------------------------------------------------------------'

            write(*, "(' ', A)", advance='no') "|Eigenval.|"
            do j = (i-1)*6+1, ceiling(i*columnas)
                write(*, "(' ', F9.5)", advance='no') eVal(d-j+1,1)
            end do
            write(*, *) ending(1:endPivot)

            write(*, "(' ', A)", advance='no') "|Energy   |"
            do j = (i-1)*6+1, ceiling(i*columnas)
                write(*, "(' ', F9.5)", advance='no') a + eVal(d-j+1,1) * b
            end do
            write(*, *) ending(1:endPivot)
            write(*, *) '--------------------------------------------------------------------------'

            write(*, "(' ', A)", advance='no') "|Occup.   |"
            do j = (i-1)*6+1, ceiling(i*columnas)
                write(*, "(' ', F9.5)", advance='no') occ(j)
            end do
            write(*, *) ending(1:endPivot)
            write(*, *) '--------------------------------------------------------------------------'
            
            write(*, *) '|  Atoms  |                                                              |'
            do k = 1, d
                write(*, "(' ', A, I4, A)", advance='no') "| ", k,"    |"
                do j = (i-1)*6+1, ceiling(i*columnas)
                    index = int(eVal(d-j+1,2))
                    write(*, "(' ', F9.5)", advance='no') eVec(k, index)
                end do
                write(*, *) ending(1:endPivot)
            end do
            write(*, *) '--------------------------------------------------------------------------'
            write(*, *) ""
        end do

        write(*, *) ""

        ! Showing all the results from the Mulliken analysis in a pretty way --------------
        do i = 1, filas

            endPivot = 2

            ending = " |"

            if ((residual /= 0) .and. (i == filas)) then  ! Formatting rows and columns
                columnas = real(d,8)/filas
                do j = 1, (6 - residual)
                    ending = "          "//ending
                    endPivot = endPivot + 10
                end do
            else
                columnas = 6
            end if

            write(*, *) '--------------------------------------------------------------------------'
            write(*, *) '|                            Mulliken Charges                            |'
            write(*, *) '--------------------------------------------------------------------------'
            write(*, "(' ', A)", advance='no') "| Atom No.|"
            do j = (i-1)*6+1, ceiling(i*columnas)
                write(*, "(' ', I9)", advance='no') j
            end do
            write(*,*) ending(1:endPivot)

            write(*, "(' ', A)", advance='no') "| Popul.  |"
            do j = (i-1)*6+1, ceiling(i*columnas)
                write(*, "(' ', F9.5)", advance='no') bondOrd(j,j)
            end do
            write(*,*) ending(1:endPivot)

            write(*, *) '--------------------------------------------------------------------------'
            write(*, *) '|                               Bond Order                               |'
            write(*, *) '--------------------------------------------------------------------------'

            do k = 1, d
                write(*, "(' ', A, I3, A)", advance='no') "|   ", k, "   |"
                do j = (i-1)*6+1, ceiling(i*columnas)
                    write(*, "(' ', F9.5)", advance='no') bondOrd(k, j)
                end do
                write(*,*) ending(1:endPivot)
            end do
            write(*, *) '--------------------------------------------------------------------------'
            write(*, *) ""
        end do

    end subroutine ShowResults

    subroutine WriteHuckel(eVal, eVec, occ, a, b, d, name_file)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to write all the results from the Huckel analysis to a file
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - eVal:       the matrix of sorted eigenvalues and their index
    ! - eVec:       the matrix of eigenvectors
    ! - occ:        a list of the occupancies of the orbitals
    ! - a:          alpha parameter for the energy
    ! - b:          beta parameter for the energy
    ! - d:          how many atoms are involved in the pi-system
    ! - name_file:  the name of the original file where the XYZ coordinates were stored
    !
    ! Outputs
    ! - in files:   FILENAME_huckel.dat will hold the eigenvalues, energies and occupancies
    !               FILENAME_eigenvectors.dat will hold a matrix with the eigenvectors
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        character(*), intent(in) :: name_file
        real(kind=8), allocatable, intent(in) :: eVal(:,:), eVec(:,:), occ(:)
        real(kind=8), intent(in) :: a, b
        integer, intent(in) :: d

        ! Internal variables
        integer :: i, j, index, fileNameLength

        ! The the actual length of the file's name
        fileNameLength = len(trim(adjustl(name_file)))

        ! Write eigenvalue, energy and occupancy to new file using the original file's name
        open(41, file=name_file(1:fileNameLength-4)//"_huckel.dat", status="unknown")

        write(41, *) "    Eigenvalue        Energy     Occupancy"
        write(41, *) "------------------------------------------"
        do i = 1, d
            write(41, "(' ', F14.8, F14.8, F14.8)") eVal(i,1), a + eVal(i,1) * b, occ(d-i+1)
        end do
        close(41)

        ! Write eigenvectors to new file using the original file's name
        open(42, file=name_file(1:fileNameLength-4)//"_eigenvectors.dat", status="unknown")
        
        do i = 1, d
            do j = 1, d
                index = int(eVal(d-j+1,2))
                write(42, "(' ', F14.8)", advance='no') eVec(i, index)
            end do
            write(42, *) ""
        end do

        close(42)

    end subroutine WriteHuckel

    subroutine WriteMulliken(bondOrd, d, name_file)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to write all the results from the Mulliken analysis to a file
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - bondOrder:  a real number matrix with the bond order of each pair of atoms
    ! - d:          how many atoms are involved in the pi-system
    ! - name_file:  the name of the original file where the XYZ coordinates were stored
    !
    ! Outputs
    ! - in files:   FILENAME_mulliken.dat will hold the bond order of each pair of atoms
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        character(*), intent(in) :: name_file
        real(kind=8), allocatable, intent(in) :: bondOrd(:,:)
        integer, intent(in) :: d

        ! Internal variables
        integer :: i, j, fileNameLength

        ! The the actual length of the file's name
        fileNameLength = len(trim(adjustl(name_file)))

        ! Write bond order matrix to new file using the original file's name
        open(43, file=name_file(1:fileNameLength-4)//"_mulliken.dat", status="unknown")

        write(43, "(A)", advance='no') " Atom "
        do i = 1, d
            write(43, "(I14)", advance='no') i
        end do
        write(43, *) ""

        do i = 1, d
            write(43, "(I6)", advance='no') i
            do j = 1, d
                write(43, "(F14.8)", advance='no') bondOrd(i,j)
            end do
            write(43, *) ""
        end do

        close(43)

    end subroutine WriteMulliken    

end module IOmethods