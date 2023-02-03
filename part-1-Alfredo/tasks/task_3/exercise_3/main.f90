! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 12:21:09 11-10-2022 |
! +--------------------------------------------+

program exercise3
    !
    ! 3. Create an array that it is a derived data type variable called
    ! “patient” that holds the name and family name of the patient, the
    ! temperature (with single precision), the pulse rate (as an integer)
    ! and a diagnosis (“healthy” or “sick”) of a given patient.
    ! Then assign the values for 3 patients and, finally, print the values
    ! for each patient to an output file. The program must ask whether the
    ! data of all or only one of the patients is going to be printed and,
    ! in the last case, ask for the family name of the searched patient.
    !
    ! Variable definition
    !
    implicit none
    character(len=80) :: numero, datos
    !
    type patient
        character(len=:), allocatable :: patname
        character(len=:), allocatable :: famname
        real(kind=4) :: temp
        integer :: pulse
        character(len=:), allocatable :: diagnosis
    end type patient
    !
    type(patient) :: jaimito, pablito, paquito
    !
    !
    ! Files
    !
    open(unit=10, file="out-exercise3.dat")
    !
    ! Formats
    !
    10 format(1x,a,1x,a,1x,f5.1,3x,i0,5x,a)
    20 format(1x,a,12x,a,2x,a,2x,a)
    30 format('-------------------------------------------')
    !
    ! === START OF THE PROGRAM ===
    !
    write(10,*) '+-------------------+'
    write(10,*) '| Program exercise3 |'
    write(10,*) '+-------------------+'
    write(10,*)
    !
    ! Define patients
    !
    jaimito = patient("Jaimito","Fernandez",34.5,35,"sick")
    pablito = patient("Pablito","Rodriguez",36.5,60,"healthy")
    paquito = patient("Paquito","Sabajanes",40.5,95,"sick")
    !
    ! Read input
    !
    write(*,*)
    write(*,*) 'The patients are:'
    write(*,*) '1. Jaimito Fernandez'
    write(*,*) '2. Pablito Rodriguez'
    write(*,*) '3. Paquito Sabajanes'
    write(*,*)
    write(*,*) 'Do you want to print the data of all the patients or only one? (Type "all" or "one")'
    read(*,*) numero
    numero = trim(numero) ! removes blank spaces
    !
    ! Write output
    !
    write(10,30)
    write(10,20) 'Patient', 'T(ºC)', 'Pulse', 'Diagnosis'
    write(10,30)
    !
    if (numero .eq. "all" .or. numero .eq. "All") then
        write(10,10) jaimito%patname, jaimito%famname, jaimito%temp, jaimito%pulse, jaimito%diagnosis
        write(10,10) pablito%patname, pablito%famname, pablito%temp, pablito%pulse, pablito%diagnosis
        write(10,10) paquito%patname, paquito%famname, paquito%temp, paquito%pulse, paquito%diagnosis
    elseif (numero .eq. "one" .or. numero .eq. "One") then
        write(*,*) 'Which patient do you want the data to be printed? Type the family name:'
        read(*,*) datos
        datos = trim(datos)
        write(*,*)
        if (datos .eq. jaimito%famname) then
            write(10,10) jaimito%patname, jaimito%famname, jaimito%temp, jaimito%pulse, jaimito%diagnosis
        elseif (datos .eq. pablito%famname) then
            write(10,10) pablito%patname, pablito%famname, pablito%temp, pablito%pulse, pablito%diagnosis
        elseif (datos .eq. paquito%famname) then
            write(10,10) paquito%patname, paquito%famname, paquito%temp, paquito%pulse, paquito%diagnosis
        else
            write(*,*) 'Wrong family name. Remember it is key sensitive.'
        end if
    else
        write(*,*) 'Wrong answer. Type "all" or "one".'
    end if
    !
    write(10,30)
    !
    write(*,*)
    write(*,*) 'Results writen to out-exercise3.dat'
    write(*,*)
    !
    stop
endprogram exercise3
