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
    use mymodule
    use jacobi_method
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
    ! TODO: change trivial matrix by Huckel matrix
    !
    ! dl1: do i = 1, 3
    !     dl2: do j = 1, 3
    !         A(i,j) = dble(i + j)
    !     end do dl2
    ! end do dl1
    !
    A(1,1) = 1.0_8
    A(1,2) = sqrt(2.0_8)
    A(1,3) = 2.0_8
    A(2,1) = A(1,2)
    A(2,2) = 3.0_8
    A(2,3) = sqrt(2.0_8)
    A(3,1) = A(1,3)
    A(3,2) = A(2,3)
    A(3,3) = 1.0_8
    !
    ! Print input
    !
    call print_input
    !
    write(unit=uf, fmt='(a)') '=== PERFORMING JACOBI METHOD ==='
    write(unit=uf, fmt=*)
    !
    call jacobi_classic(A, tolerance, v, ev, totiter)
    !
    n = size(A, dim=1)
    !
    call print_output
    !
    write(unit=uf, fmt='("Program finished successfully.")')
    write(unit=uf, fmt='("Bye!")')
    stop
endprogram Huckel_Hamiltonian
