! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Friday 17:00:46 09-12-2022 |
! +-------------------------------------------+

program NumInt_exercise_1
    !
    ! Fortran program to solve numerically the integral
    !
    ! \int_{-1}^{1} \sin(x+1)
    !
    ! with the Newton-Cotes methods, specifically the
    ! Rectangle method: Simple and Composite rule
    !
    ! Modules
    !
    use io
    use mymodule
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+---------------------------+'
    write(*,*) '| Program NumInt_exercise_1 |'
    write(*,*) '+---------------------------+'
    write(*,*)
    !
    ! Read input
    !
    call read_input
    !
    write(*,*) 'Lower integration limit: ', lil
    write(*,*) 'Upper integration limit: ', uil
    write(*,*)
    !
    ! Ask for the results to be written in the screen or in a separated file
    !
    write(*,*) 'Do you want to print the results in the screen, in a file or'
    write(*,*) 'both? [screen/file/both]'
    read(*,'(A)') printres
    printres = trim(printres)
    !
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
    !  +- (+1)
    !  |       sin(x + 1) dx
    ! -+  (-1)
    !
    100 continue ! Label to repeat calculations
    !
    ! --- Simple rule ---
    !
    call SimpleNCRM(lil, uil, IR, uf)
    !
    ! --- Composite rule ---
    !
    call CompositeNCRM(lil, uil, threshold, IRC, uf)
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
    ! Print ending message
    !
    write(*,*)
    write(*,*) 'Program compiled successfully.'
    write(*,*)
    write(*,*) 'Bye!'
    !
    ! Close output file
    !
    close(uf)
    !
    stop
endprogram NumInt_exercise_1
