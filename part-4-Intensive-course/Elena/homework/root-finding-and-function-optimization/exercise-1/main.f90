! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 16:50:27 17/01/2023 |
! +--------------------------------------------+

program FuncOpt_exercise_1
    !
    ! Fortran program to minimize the following two-dimensional function
    !
    ! $f(x,y) = \sin(x+y) + (x - y)^2 - 1.5x + 3.5y + 3$
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
    write(*,'(A)') '| Program FuncOpt_exercise_1 |'
    write(*,'(A)') '+----------------------------+'
    write(*,*)
    !
    ! Read input
    !
    call read_input
    !
    write(unit=*, fmt='(A,i0)') 'Number of coordinates: ', ndim
    write(unit=*, fmt='(A,i0)') 'Maximum number of iterations: ', maxiter
    write(unit=*, fmt='(A,*(f8.2))') 'Initial points: ', initcoord
    write(unit=*, fmt='(A,f8.2)') 'Fixed step size, gamma: ', gamma
    write(unit=*, fmt='(A,f8.2)') 'Value of h for the finite difference: ', h
    write(unit=*, fmt='(A,A)') &
    & 'Method of the finite difference: ', fin_diff_method
    !
    ! Steepest Descent method
    !
    call steepest_descent(initcoord, gamma, maxiter, threshold, &
    & fin_diff_method, h, coord, grad, normgrad, mincoord, func, totiter)
    !
    ! --- Print results ---
    !
    uf = 6
    call print_output
    !
    ! Print ending message
    !
    write(*,*)
    write(*,'(a)') 'Program compiled successfully.'
    write(*,*)
    write(*,'(a)') 'Bye!'
    !
    stop
endprogram FuncOpt_exercise_1
