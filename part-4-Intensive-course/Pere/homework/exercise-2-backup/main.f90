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
    !
    ! TODO: ask for file/screen to write the results
    !
    uf=6
    !
    ! === START OF THE PROGRAM ===
    !
    write(uf,'(a)') '+----------------------------+'
    write(uf,'(a)') '| Program Huckel_Hamiltonian |'
    write(uf,'(a)') '+----------------------------+'
    write(uf,*)
    !
    ! Read input
    !
    call read_input
    !
    ! Print input
    !
    call Huckel_matrix(natoms, alpha, beta, H)
    !
    call print_input
    !
    ! Calculate the eigenvalues and eigenvectors with the Jacobi method
    !
    call jacobi_classic(H, tolerance, v, ev, totiter)
    !
    call Huckel_evals_evecs(ev, v)
    !
    call print_output
    !
    ! Values calculated from the slides
    !
    write(unit=uf, fmt='("Program finished successfully.")')
    write(unit=uf, fmt='("Bye!")')
    stop
endprogram Huckel_Hamiltonian
