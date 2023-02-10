! =========================================================================================
!                         Huckel Method for Pi Orbitals in Molecules
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

program Huckel
! -----------------------------------------------------------------------------------------
! This program does a Huckel and Mulliken analysis for a molecule with a resonant pi
! electron system, read from an XYZ file
! -----------------------------------------------------------------------------------------
!
! Inputs:
! - file:      A file with the symbols and XYZ coordinates of each atom in the molecule
! - charge:    The charge of the molecule
!
! Outputs:
! - on screen: The results of the Huckel and Mulliken analyses for a given molecule
! - as a file: 3 files - 2 for the Huckel analysis and 1 for the Mulliken analysis
!
! -----------------------------------------------------------------------------------------

	! Import the integration modules ------------------------------------------------------
    use LAmethods, only : HuckMat, Jacobi, Mulliken, Occupancy, QuickSort, ShowMat
    use IOmethods, only : ReadXYZ, removeHs, ShowResults, WriteHuckel, WriteMulliken

    implicit none

    ! Variables ---------------------------------------------------------------------------
    character(len=2), allocatable :: atoms(:), atoms2(:)
    real(kind=8), allocatable :: XYZ(:,:), XYZ2(:,:), Huck(:,:), eVec(:,:), eVal(:)
    real(kind=8), allocatable :: eValSorted(:,:), bondOrder(:,:), mulCharge(:), occ(:)
    character(100) :: name_file, dummyCharge, dummyAlpha, dummyBeta
    real(kind=8) :: alfa, beta, charge
    integer :: i, j, n, clargs

    ! Providing some GUI to the user ------------------------------------------------------
    write(*, *) '--------------------------------------------------------------------------'
    write(*, *) '|                        Hückel π-orbital Analysis                       |'
    write(*, *) '|                                   &                                    |'
    write(*, *) '|                      Mulliken Population Analysis                      |'
    write(*, *) '--------------------------------------------------------------------------'
    write(*, *) '|                                                                        |'
    write(*, *) '|        This program will compute the Huckel and Mulliken               |'
    write(*, *) '|        analysis for a molecule with a resonant π-electron              |'
    write(*, *) '|        system, read from an XYZ file.                                  |'
    write(*, *) '|                                                                        |'
    write(*, *) '|        You will have to enter the charge of the molecule and           |'
    write(*, *) "|        the name of the XYZ file.                                       |"
    write(*, *) "|                            ___         __                              |"
    write(*, *) "|                          .`   `.     .`   `.                           |"
    write(*, *) "|                         /       \   /       \                          |"
    write(*, *) "|                        |     *   `-`         |                         |"
    write(*, *) "|                        |         .-.   *     |                         |"
    write(*, *) "|                         \       /   \       /                          |"
    write(*, *) "|                          \. - ./     \. - ./                           |"
    write(*, *) "|                      _  . \   / ` . ` \   / .  _                       |"
    write(*, *) "|                    .` `*   \ /   * .   \ /   *` `.                     |"
    write(*, *) "|                   (  H |)   C    | |    C   (| H  )                    |"
    write(*, *) "|                    `._.*   / \   . *   / \   *._.`                     |"
    write(*, *) "|                         . /   \   .   /   \ .                          |"
    write(*, *) "|                          /‧ - ‧\′   `/‧ - ‧\                           |"
    write(*, *) "|                         /    *  \   /       \                          |"
    write(*, *) "|                        |         `-`         |                         |"
    write(*, *) "|                        |         .-.  *      |                         |"
    write(*, *) "|                         \       /   \       /                          |"
    write(*, *) "|                          `.___.`     `.___.`                           |"
    write(*, *) "|                                                                        |"
    write(*, *) '|                                                                        |'

    ! Check if the user provided command line arguments
    clargs = COMMAND_ARGUMENT_COUNT()
    if (clargs == 4) then ! If so, extract the arguments from the file
        write(*, *) "|                                                                        |"
        write(*, *) "|  Running in command line mode ...                                      |"
        write(*, *) '|                                                                        |'
        call GET_COMMAND_ARGUMENT(1,name_file)    ! Extract the name of the XYZ file
        call GET_COMMAND_ARGUMENT(2,dummyCharge)  ! Extract the charge of the molecule
        read(dummyCharge, *) charge               ! Parse the string into an integer
        call GET_COMMAND_ARGUMENT(3,dummyAlpha)   ! Extract the parameter alpha
        read(dummyAlpha, *) alfa                  ! Parse the string into a real number
        call GET_COMMAND_ARGUMENT(4,dummyBeta)    ! Extract the parameter beta
        read(dummyBeta, *) beta                   ! Parse the string into an integer

    ! If the user has no clue and failed to enter the correct command line arguments
    else if (((clargs > 4) .or. ((clargs < 4) .and. (clargs > 0))) .or. (clargs == 1)) then
        write(*, *) "|                                                                        |"
        write(*, *) "|  WARNING: Input error                                                  |"
        write(*, *) "|                                                                        |"
        write(*, *) "|  To use command line arguments, please use the following format:       |"
        write(*, *) "|                                                                        |"
        write(*, *) "|                                                                        |"
        write(*, *) "|  $ ./huckel.exe  name_of_file.xyz  charge  α  β                        |"
        write(*, *) "|                                                                        |"
        write(*, *) "|                                                                        |"
        write(*, *) "|  ./huckel.exe      : the program to be run                             |"
        write(*, *) "|                                                                        |"
        write(*, *) "|  name_of_file.xyz  : the name of the file with the xyz coordinates     |"
        write(*, *) "|                                                                        |"
        write(*, *) "|  charge            : the charge of the molecule as an integer (eg. -1) |"
        write(*, *) "|                                                                        |"
        write(*, *) "|  α                 : the first energy parameter as a real number       |"
        write(*, *) "|                                                                        |"
        write(*, *) "|  β                 : the second energy parameter as a real number      |"
        write(*, *) "|                                                                        |"
        write(*, *) "|                                                                        |"
        write(*, *) "|  Please try again, with the correct format.     Exiting ...            |"
        write(*, *) "|                                                                        |"
        write(*, *) '--------------------------------------------------------------------------'
        stop
    ! If the user provided no command line arguments, run in interactive mode
    else
        write(*, *) "|                                                                        |"
        write(*, *) "|  Running in interactive mode ...                                       |"
        write(*, *) '|                                                                        |'
        write(*, "(' ', A)", advance='no') '|        Please enter name of the XYZ file: '
        read(*, *) name_file
        write(*, *) '|                                                                        |'
        write(*, "(' ', A)", advance='no') '|        Please enter charge of the molecule: '
        read(*, *) charge
        write(*, *) '|                                                                        |'
        write(*, "(' ', A)", advance='no') '|        Please enter α parameter for the energy [default -11.4]: '
        read(*, *) alfa
        write(*, *) '|                                                                        |'
        write(*, "(' ', A)", advance='no') '|        Please enter β parameter for the energy [default -0.8]: '
        read(*, *) beta
    end if

    write(*, *) "|                                                                        |"
    write(*, *) "|   Accepted values:                                                     |"
    write(*, *) "|                                                                        |"
    write(*, "(' ', A15, F7.2, A)") "|   Charge =   ", charge, "                                                   |"
    write(*, *) "|                                                                        |"
    write(*, "(' ', A15, F8.2, A)") "|   α      =   ", alfa, "                                                   |"
    write(*, *) "|                                                                        |"
    write(*, "(' ', A15, F8.2, A)") "|   β      =   ", beta, "                                                   |"
    write(*, *) "|                                                                        |"
    write(*, *) '--------------------------------------------------------------------------'
    write(*, *) '|                                                                        |'
    write(*, *) '|                               Computing ...                            |'
    write(*, *) '|                                                                        |'
    write(*, *) '--------------------------------------------------------------------------'

    ! Read the XYZ file
    call ReadXYZ(name_file, atoms, XYZ)

    ! Remove the Hydrogen atoms
    call removeHs(atoms, XYZ, atoms2, XYZ2)

    ! Build the Huckel matrix
    call HuckMat(XYZ2, Huck)

    ! Compute eigenstates of Huckel
    call Jacobi(Huck, eVal, eVec)

    ! Get the number of eigenstates
    n = size(atoms2)

    ! Assign memory to matrix of sorted eigenstates
    allocate(eValSorted(n,2))

    do i = 1, n
        eValSorted(i,1) = eVal(i)            ! Copying the eigenvalues
        eValSorted(i,2) = i                  ! Using a pointer to keep track
    end do

    ! Sort eigenstates (but keep track)
    call QuickSort(eValSorted)

    ! Compute the electron occupancy of each atom
    call Occupancy(eValSorted, charge, occ)

    ! Compute the Bond Order matrix and the Mulliken charges for each atom
    call Mulliken(eValSorted, eVec, occ, bondOrder, mulCharge)

    ! Display the results in a human-readable way
    call ShowResults(eValSorted, eVec, occ, alfa, beta, n, bondOrder)

    ! Save all the Huckel results to a file
    call WriteHuckel(eValSorted, eVec, occ, alfa, beta, n, name_file)

    ! Save all the Mulliken results to a file
    call WriteMulliken(bondOrder, n, name_file)

end program Huckel