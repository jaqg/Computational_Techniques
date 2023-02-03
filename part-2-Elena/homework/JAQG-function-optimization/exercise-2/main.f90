! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 00:48:51 18-12-2022 |
! +-------------------------------------------+

program FuncOpt_exercise_2
    !
    ! General Fortran program to find the minimum of one-dimensional function
    !
    ! f(x) = x^2 - x
    !
    ! using Newton-Raphson method and finite difference methods to compute
    ! the derivatives
    !
    ! Modules
    !
    use io
    use mymodule
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+----------------------------+'
    write(*,*) '| Program FuncOpt_exercise_1 |'
    write(*,*) '+----------------------------+'
    write(*,*)
    !
    ! Read input
    !
    call read_input
    !
    ! Newton-Raphson method
    !
    write(*,*) 'Method of the finite difference: ', fin_diff_method
    write(*,*)
    write(*,'(a,2x,a,19x,a,14x,a)') 'Iter.', 'x', 'f(x)', 'grad.'
    !
    call Newton_Raphson(fin_diff_method, x0, h, threshold, totiter, xmin)
    !
    write(*,*)
    write(*,'(a,i0)') 'Total number of iterations: ', totiter
    write(*,*)
    write(*,'(a, f12.8)') 'Position of minimum: xmin =', xmin
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
