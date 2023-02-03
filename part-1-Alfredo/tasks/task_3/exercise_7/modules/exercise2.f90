subroutine exercise2(inunfile, filename, colrow, skiplines, &
                     outunfile, outform)
    !
    ! Subroutine that reads 10 (real) numbers from the #inunfile (int)
    ! file named "filename", prints in out-unitfile #outunfile those
    ! bigger than 5.0 and writes -5.0 instead of the numbers lower or
    ! equal than 5.0.
    ! With colrow, you can write "col" if the numbers are sorted in a
    ! column in the input file, or "row" if they are in one single line
    ! Also, skips #skiplines (int) at the beginning of the file.
    !
    implicit none
    !
    integer, intent(in) :: inunfile, skiplines, outunfile
    character(len=*), intent(in) :: filename
    character(len=3), intent(in) :: colrow
    character(len=*), intent(in) :: outform
    character(len=:), allocatable :: actual_format
    integer :: i
    real(kind=8), dimension(10) :: numbers
    !
    open(unit=inunfile, file=filename, status='old', action='read')
    !
    l1: do i = 1, skiplines
        read(inunfile,*)
    end do l1
    !
    if (colrow == "col") then
        l2: do i = 1, 10
            read(inunfile,*) numbers(i)
        end do l2
    else
        read(inunfile,*) (numbers(i),i=1,10)
    end if
    !
    write(outunfile,*) 'The original numbers are:'
    if (outform == "*") then
        do i = 1, 10
            write(outunfile,*) numbers(i)
        end do
    else
        actual_format = "(*(" // outform // "))"
        do i = 1, 10
            write(outunfile,actual_format) numbers(i)
        end do
    end if
    !
    write(outunfile,*)
    !
    write(outunfile,*) 'The conditioned numbers are:'
    if (outform == "*") then
    !
        do i = 1, 10
            if (numbers(i)<=5.0) then
                write(outunfile,*) -5.0
            else
                write(outunfile,*) numbers(i)
            end if
        end do
    !
    else
    !
        actual_format = "(*(" // outform // "))"
        do i = 1, 10
            if (numbers(i)<=5.0) then
                write(outunfile,actual_format) -5.0
            else
                write(outunfile,actual_format) numbers(i)
            end if
        end do
    !
    end if
    !
    write(outunfile,*)
    !
    close(inunfile)
    !
    return
end subroutine exercise2
