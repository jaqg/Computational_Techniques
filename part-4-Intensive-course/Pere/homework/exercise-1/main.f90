! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 00:20:12 01/03/2023 |
! +----------------------------------------------+

program least_squares_fit
    !
    ! Program to compute a polynomial fit to a set of data points with 
    ! the Gauss-Jordan method.
    !
    ! Modules
    !
    use io
    use polynomial_regression
    use gauss_jordan
    !
    ! TODO: ask for file/screen to write the results
    !
    uf=6
    fmt = 'f15.4'
    fmt = trim(fmt)
    !
    ! === START OF THE PROGRAM ===
    !
    write(uf,'(a)') '+---------------------------+'
    write(uf,'(a)') '| Program least_squares_fit |'
    write(uf,'(a)') '+---------------------------+'
    write(uf,*)
    !
    ! Read input
    !
    call read_input
    !
    ! Print input data
    !
    call print_input
    write(uf,*)
    !
    ! Calculations
    !
    write(uf,'(a)') '+---------------------------+'
    write(uf,'(a)') '|          OUTPUT           |'
    write(uf,'(a)') '+---------------------------+'
    write(uf,*)
    !
    ! Create the augmented matrix
    !
    call create_augmented_mat(datmat, poldeg, augmat)
    !
    ! Write the augmented matrix
    !
    write(uf,'(a)') 'Augmented matrix:'
    call write_mat(augmat, uf, fmt, size(augmat, dim=2))         
    write(uf,*)
    !
    ! Perform the Gauss elimination over the augmented matrix
    !
    call gauss_elimination(augmat, tolerance)
    !
    ! Reduce the upper triangular matrix to the diagonal (Jordan) or perform
    ! backsubstitution
    !
    write(uf,'(*(a))') 'Elimination method: ', elim_method
    write(uf,*)
    !
    if (elim_method .eq. 'Jordan' .or. elim_method .eq. 'jordan') then 
        !
        ! Perform Gauss-Jordan
        !
        call jordan(augmat, coefs)
        !
    elseif (elim_method .eq. 'Backsubstitution' .or. &
        & elim_method .eq. 'backsubstitution') then 
        !
        ! Perform backsubstitution
        !
        call back_substitution(augmat(:,1:poldeg+1), augmat(:,poldeg+2), coefs)
        !
    else
        write(*,*) 'ERROR: incorrect elimination method.'
    end if
    !
    ! Print the result matrix
    !
    write(uf,'(a)') 'Result matrix:'
    call write_mat(augmat, uf, fmt, size(augmat, dim=2))         
    write(uf,*)
    !
    ! Print the coefficient vector
    !
    write(uf,'(a)') 'Coefficients vector:'
    call write_vec(coefs, uf, 'f15.8')
    write(uf,*)
    !
    ! Print the value of R^2
    !
    call R_coeff(datmat(:,1), datmat(:,2), coefs, R2)
    write(uf,'(a, f8.6)') 'R^2 = ', R2
    write(uf,*)
    !
    ! Write data to plot into output file './data/out-graph.dat'
    !
    call write_plot_data
    write(uf,'(a)') "Results written to output files in ./data/ folder."
    write(uf,*)
    ! !
    ! ! Pruebas para la inversa
    ! !
    ! do i = 1, 3
    !     do j = 1, 3
    !         prueba(i,j) = dble(i + j)
    !     end do 
    ! end do 
    ! !
    ! write(*,*) 'PRUEBA MATRIZ INVERSA'
    ! write(*,*)
    ! write(*,*) 'A ='
    ! call write_mat(prueba, 6, fmt, size(prueba, dim=2))         
    ! write(*,*)
    ! !
    ! call inv_mat_gauss(prueba, tolerance, pruebainv)
    !
    ! Print the ending message
    !
    write(unit=uf, fmt='("Program finished successfully.")')
    write(unit=uf, fmt='("Bye!")')
    stop
endprogram least_squares_fit
