! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 23:00:56 25/02/2023 |
! +---------------------------------------------+

program NumInt_exercise_1
    !
    ! Fortran program to solve numerically the integral
    !
    ! $\int_{1}^{3} \sin(x^2) - \cos(2x) \dd{x}$
    !
    ! with the composite Simpson rule
    !
    ! Modules
    !
    use io
    use mymodule
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,'(A)') '+---------------------------+'
    write(*,'(A)') '| Program NumInt_exercise_1 |'
    write(*,'(A)') '+---------------------------+'
    write(*,*)
    !
    ! Read input
    !
    call read_input
    !
    ! Print input
    !
    call print_input
        !
    ! Ask for the results to be written in the screen or in a separated file
    !
    write(*,*) 'Do you want to print the results in the screen, in a file or'
    write(*,*) 'both? [screen/file/both]'
    read(*,'(A)') printres
    printres = trim(printres)

    ! If selected
    ! 'screen' -> set unitfile (of output file) to 6
    ! 'file'   -> set unitfile (of output file) to a -random- new unitfile
    ! 'both'   -> write the results in the screen and
    !             set unitfile (of output file) to a -random- new unitfile
    !
    if (printres=='screen' .or. printres=='Screen') then
        uf = 6
    elseif (printres=='file' .or. printres=='File') then
        open(newunit=uf, file="data/output.dat", action='write')
    elseif (printres=='both' .or. printres=='Both') then
        dummyvar = 1
        open(newunit=uf, file="data/output.dat", action='write')
    else
        write(*,*) 'Error main.f90: bad input.&
                  & Options: "screen", "file" or "both".'
    end if
    !
    ! Solve numerically the integral
    !
    !  +- (3)
    !  |       \sin(x^2) - \cos(2x) dx
    ! -+  (1)
    !
    100 continue ! Label to repeat calculations
    !
    ! --- Simpson composite rule ---
    !
    call SimpsonCompositeNCM(lil, uil, initN, threshold, IRC, totiter, uf)
    !
    ! In case of printing both to screen and output file, repeat
    !
    is1: if (printres=='both' .or. printres=='Both') then
        !
        ! I define a dummy variable, 'dummyvar', that
        ! = 1 in the "first cycle" when printing both to screen & file,
        ! and updates to 2 after printing results to file so it doesn't
        ! cycle infinitely
        !
        if (dummyvar == 1) then
            uf = 6
            dummyvar = 2
            goto 100
        else
            exit is1
        end if
    end if is1
    !
    ! In case of printing to output file, specify it
    !
    is2: if (printres=='file' .or. printres=='File') then
        write(unit=*, fmt='(A)') 'Results written to data/output.dat'
    end if is2
    !
    ! Print ending message
    !
    write(*,*)
    write(*,'(A)') 'Program compiled successfully.'
    write(*,*)
    write(*,'(A)') 'Bye!'
    !
    stop
endprogram NumInt_exercise_1
