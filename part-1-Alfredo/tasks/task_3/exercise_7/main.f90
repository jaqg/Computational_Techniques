! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 16:46:21 16-10-2022 |
! +-------------------------------------------+

program exercise7
    !
    ! 7. Build a library containing all the subprograms (functions or
    ! subroutines) created until now. The simple programs written in the
    ! first exercises must be converted into subprograms and also be
    ! included in the library
    !
    ! Modules
    !
    use patients_mod
    use mat_mod
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: res1, res2
    integer :: int_div, nint_div
    !
    type(patient) :: jaimito, pablito, paquito
    !
    ! Files
    !
    !
    ! Formats
    !
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-------------------+'
    write(*,*) '| Program exercise7 |'
    write(*,*) '+-------------------+'
    write(*,*)
    !
    ! Subroutines/functions of exercise-1
    !
    res1 = int_div(1.0_8, 2.0_8)
    res2 = nint_div(1.0_8, 2.0_8)
    !
    write(*,'(1x,a,i0)') ' int_div(1.0/2.0) = ', res1
    write(*,'(1x,a,i0)') 'nint_div(1.0/2.0) = ', res2
    write(*,*)
    !
    ! Subroutines/functions of exercise-2
    !
    call exercise2(10, "in-exercise2.dat", "col", 1, 6, "F10.2")
    !
    ! Subroutines/functions of exercise-3
    !
    jaimito = patient("Jaimito","Fernandez",34.5,35,"sick")
    pablito = patient("Pablito","Rodriguez",36.5,60,"healthy")
    paquito = patient("Paquito","Sabajanes",40.5,95,"sick")
    !
    call patients_print(jaimito, "yes", 11, "out-exercise3.dat", "no")
    call patients_print(pablito, "no", 11, "out-exercise3.dat", "no")
    call patients_print(paquito, "no", 11, "out-exercise3.dat", "yes")
    !
    write(*,*) 'Results stored in "out-exercise3.dat".'
    write(*,*)
    !
    stop
endprogram exercise7
