module patients_mod
    !
    implicit none
    !
    type patient
        character(len=:), allocatable :: patname
        character(len=:), allocatable :: famname
        real(kind=4) :: temp
        integer :: pulse
        character(len=:), allocatable :: diagnosis
    end type patient
    !
    contains
        subroutine patients_print(pname, header, outfun, outfname, foot)
            !
            ! Print the values for each patient to an output file.
            ! The program must ask whether the data of all or only one of the
            ! patients is going to be printed and, in the last case, ask for
            ! the family name of the searched patient.
            !
            ! patname (string): patient name
            ! header (string): print header of table: "yes" or "no"
            ! outfun (integer): output file unit
            ! outfname (string): output file name
            ! foot (string): print foot of table: "yes" or "no"
            !
            implicit none
            !
            class(patient), intent(in) :: pname
            character(len=*), intent(in) :: header, foot
            integer, intent(in) :: outfun
            character(len=*), intent(in) :: outfname
            !
            10 format(1x,a,1x,a,1x,f5.1,3x,i0,5x,a)
            20 format(1x,a,12x,a,2x,a,2x,a)
            30 format('-------------------------------------------')
            !
            open(unit=outfun, file=outfname, action='write')
            !
            if (header == "yes") then
                write(outfun,30)
                write(outfun,20) 'Patient', 'T(ºC)', 'Pulse', 'Diagnosis'
                write(outfun,30)
            end if
            !
            write(outfun,10) pname%patname, pname%famname, pname%temp, &
                             pname%pulse, pname%diagnosis
            !
            if (foot == "yes") then
                write(outfun,30)
            end if
            !
            return
        end subroutine patients_print
end module
