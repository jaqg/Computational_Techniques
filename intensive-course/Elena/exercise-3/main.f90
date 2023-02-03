! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 16:23:47 17/01/2023 |
! +--------------------------------------------+

program FuncOpt_exercise_1
    !
    ! Fortran program to find the root of one-dimensional function
    !
    ! f(x) = 3e^x - 4cos(x)
    !
    ! using one-dimensional Newton-Raphson method
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
    ! Newton-Raphson method
    !
    write(*,*) 'Method of the finite difference: ', fin_diff_method
    write(*,*)
    write(*,'(a,2x,a,14x,a,11x,a)') 'Iter.', 'x', 'f(x)', 'grad.'
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
endprogram FuncOpt_exercise_1
