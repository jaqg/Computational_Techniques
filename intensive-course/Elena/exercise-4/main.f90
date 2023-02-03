! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 16:50:27 17/01/2023 |
! +--------------------------------------------+

program FuncOpt_exercise_2
    !
    ! Fortran program to minimize the following two-dimensional function
    !
    ! f(x,y) = 25x^2 + y^2
    !
    ! using the Steepest Descent method
    !
    ! Modules
    !
    use io
    use mymodule
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,'(A)') '+----------------------------+'
    write(*,'(A)') '| Program FuncOpt_exercise_2 |'
    write(*,'(A)') '+----------------------------+'
    write(*,*)
    !
    ! Read input
    !
    call read_input
    !
    write(unit=*, fmt='(A,i0)') 'Number of coordinates: ', ncoord
    write(unit=*, fmt='(A,i0)') 'Maximum number of iterations: ', maxiter
    write(unit=*, fmt=*) ''
    !
    ! Steepest Descent method
    !
    ! 1. Choose an initial starting point P0
    !
    allocate(coord(maxiter,ncoord), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of coord'
    !
    coord(1,1) = x0
    coord(1,2) = y0
    !
    write(unit=*, fmt='(A,f3.1,A,f3.1,A)') &
    & 'Initial starting point: P0 = (', coord(1,1), ', ', coord(1,2), ')'
    write(unit=*, fmt=*) ''
    !
    ! 2. Computation of the steepest direction of f(P)
    !
    !
    ! 3. Calculation of the gradient at P0
    !
    !
    allocate(grad(maxiter,ncoord), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of grad'
    !
    call finite_diff(fin_diff_method, 1, coord(1,1), coord(1,2), h, &
                    & grad(1,1), grad(1,2))
    !
    write(unit=*, fmt='(A)') 'Gradient at P0 ='
    write(unit=*, fmt='(*(f13.8))') grad(1,:)
    !
    ! 4. Evaluate the next point P_{k+1}
    !
    ! call finite_diff(method, 1, P0(1), P0(2), h, coord(1,1), coord(1,2))
    !
    ! 5. Repeat until convergence
    !
    !
    ! --- Print results ---
    !
    ! write(*,*)
    ! write(*,'(a,i0)') 'Total number of iterations: ', totiter
    ! write(*,*)
    ! write(*,'(a, f12.8)') 'Position of minimum: xmin =', xmin
    !
    ! Print ending message
    !
    write(*,*)
    write(*,*) 'Program compiled successfully.'
    write(*,*)
    write(*,*) 'Bye!'
    !
    stop
endprogram FuncOpt_exercise_2
