! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 11:40:24 03-12-2022 |
! +---------------------------------------------+

program NumInt_exercise_2
    !
    ! Fortran program to calculate numerically the pi number
    !
    !
    ! Modules
    !
    use io
    use mymodule
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+---------------------------+'
    write(*,*) '| Program NumInt_exercise_2 |'
    write(*,*) '+---------------------------+'
    write(*,*)
    !
    ! Read input
    !
    call read_input
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
        call numcalcpi(1000000_8, h, Ncircle, Ntotal, pical, errpi, 6)
        open(newunit=uf, file="data/output.dat", action='write')
    else
        write(*,*) 'Error main.f90: bad input.&
                  & Options: "screen", "file" or "both".'
    end if
    !
    ! Calculate numerically "approximate" value of pi
    !
    call numcalcpi(1000000_8, h, Ncircle, Ntotal, pical, errpi, uf)
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
endprogram NumInt_exercise_2
