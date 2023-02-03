! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 00:48:51 18-12-2022 |
! +-------------------------------------------+

program FuncOpt_exercise_1
    !
    ! General Fortran program to find the roots of the one-dimensional function
    !
    ! f(x) = e^{-x} (3.2 sin(x) - 0.5 cos(x) )
    !
    ! using Bolzano's methods:
    ! (a) Bisection method
    ! (b) Regula Fasi mehtod
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
    ! Check if the values of the function at those points are of different sign
    ! otherwise, ask for two new starting points
    !
    if (f(a)<0 .and. f(b)<0 .or. f(a)>0 .and. f(b)>0) then
        !
        write(*,*) 'f(a) and f(b) are the same sign. Please, introduce'
        write(*,*) 'two new starting points (a, b)'
        !
        write(*,*) 'a?'
        read(*,*) a
        !
        write(*,*) 'b?'
        read(*,*) b
        !
    end if
    !
    ! If in the interval [a,b] the function is crecient, keep the interval as
    ! [a,b]. If it is decrecient, then "flip the function" with respect to
    ! its root, and consider the interval as [b,a], so the function is always
    ! crecient and we can apply the same algorithm for any case.
    !
    if (f(a) > 0 .and. f(b) < 0) then ! [a,b] -> [b,a]
        atmp = a
        a = b
        b = atmp
    end if
    !
    ! (a) Bisection method
    !
    call Bolzano('Bisection', a, b, threshold, res_Bisec, totiter_Bisec)
    !
    !
    ! (b) Regula Falsi method
    !
    call Bolzano('Regula_Falsi', a, b, threshold, res_RF, totiter_RF)
    !
    ! Write output
    !
    write(*,*) 'Do you want to print output in the screen, file or both?'
    write(*,*) '[screen/file/both]'
    read(*,*) wheretoprint
    wheretoprint = trim( wheretoprint )
    write(*,*)
    !
    call write_output(wheretoprint)
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
