! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 10:18:14 25/01/2023 |
! +----------------------------------------------+

program Huckel_Hamiltonian
    !
    ! 
    ! Modules
    !
    use io
    use amymodule
    use jacobi_method
    use Huckel_method
    use Mulliken_pop_analysis
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,'(a)') '+----------------------------+'
    write(*,'(a)') '| Program Huckel_Hamiltonian |'
    write(*,'(a)') '+----------------------------+'
    write(*,*)
    !
    ! Read input
    !
    uf = 6
    call read_input
    !
    ! Read XYZ file
    !
    call read_XYZ(xyzfile, natoms, symbols, xyz)
    !
    ! Generate distance matrix
    !
    call distance_matrix(xyz, dist_mat)
    !
    ! Generate the topological matrix
    !
    call topological_matrix(dist_mat, adjency, symbols, topological_mat)
    nCatoms = size(topological_mat, dim=1)
    !
    ! Generate the Hamiltonian matrix for the Hückel method
    !
    call Huckel_matrix(topological_mat, alpha, beta, H)
    !
    ! Calculate the eigenvalues and eigenvectors with the Jacobi method
    !
    allocate(H_diag(size(H, dim=1), size(H, dim=2)), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of H_diag'
    !
    H_diag = H
    call jacobi_classic(H_diag, tolerance, v, ev, totiter)
    !
    ! Sort eigenvalues/eigenvectors and normalise
    !
    call Huckel_evals_evecs(ev, v)
    !
    ! Perform Mulliken population analysis
    !
    call Mulliken_population(charge, nCatoms, ev, v, &
        & npielec, EHuckel, n_occ, n_occ_pi, pi_BO)
    !
    ! Print input & output
    !
    call print_input
    call print_output
    !
    ! Write results also to output file
    !
    outputfn = xyzfile(1:len(trim(xyzfile))-4)//'-out.dat'
    open(newunit=ofu, file='data/'//outputfn, iostat=ios, action="write")
    if (ios /= 0) stop "Error opening file outputfn"
    !
    uf = ofu
    !
    call print_input
    call print_output
    !
    write(*,'(*(a))') 'Results written to '//outputfn
    write(*,*)
    !
    close(ofu)
    !
    ! Values calculated from the slides
    !
    write(unit=*, fmt='("Program finished successfully.")')
    write(unit=*, fmt='("Bye!")')
    stop
endprogram Huckel_Hamiltonian
