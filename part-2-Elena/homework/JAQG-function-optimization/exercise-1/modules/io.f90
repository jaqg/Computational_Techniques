! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 00:48:51 18-12-2022 |
! +-------------------------------------------+

module io
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, totiter_Bisec, totiter_RF
    real(kind=8) :: a, atmp, b, c, threshold, res_Bisec, res_RF
    character(len=80) :: wheretoprint
    !
    ! Subprograms
    !
    contains
        subroutine read_input
            !
            ! Subroutine to read input
            !
            implicit none
            !
            integer :: uf
            !
            open(newunit=uf,file="data/input.dat",action="read",status='old')
            !
            ! Read value of 'a' for the interval [a, b] of the
            ! Bolzano's algorithm
            !
            read(uf,*)
            read(uf,*) a
            !
            ! Read value of 'b' for the interval [a, b] of the
            ! Bolzano's algorithm
            !
            read(uf,*)
            read(uf,*) b
            !
            ! Read value of threshold for convergence
            !
            read(uf,*)
            read(uf,*) threshold
            !
            ! Close the file
            !
            close(uf)
            !
            return
        end subroutine read_input
        !
        subroutine write_output(media)
            !
            ! Subroutine to write output
            !
            ! media     character
            !
            ! media: where to print the output (screen/file)
            !
            implicit none
            !
            character(len=*), intent(in) :: media
            integer :: uf, dummyvar=0
            !
            ! If output is desired to be printed in the screen, set the
            ! unitfile to uf=6. Else case, create the file 'data/output.dat'
            ! and print the output in this file
            !
            if (media=='file' .or. media=='File') then
                open(newunit=uf,file="data/output.dat",action="write")
            elseif (media=='screen' .or. media=='Screen') then
                uf = 6
            elseif (media=='both' .or. media=='Both') then
                dummyvar = 1
                open(newunit=uf,file="data/output.dat",action="write")
            else
                write(*,*) 'write_output.f90: ERROR bad input'
            end if
            !
            10 continue
            dummyvar = dummyvar + 1
            !
            ! Write the values of [a,b] and the threshold
            !
            write(uf,*) 'Value of "a" and "b" of the interval [a,b] and'
            write(uf,*) 'threshold of convergence'
            write(uf,'(a, f5.2)') 'a =', a
            write(uf,'(a, f5.2)') 'b =', b
            write(uf,'(a, e8.1)') 'threshold =', threshold
            write(uf,*)
            !
            ! Write the results
            !
            write(uf,*) 'The root (x for f(x) approx. 0), is:'
            write(uf,*)
            !
            write(uf,*) 'Bisection method:'
            write(uf,'(a, f12.8)') 'x =', res_Bisec
            write(uf,'(a,i0)') 'Total number of iterations: ', totiter_Bisec
            write(uf,*)
            !
            write(uf,*) 'Regula Falsi method:'
            write(uf,'(a, f12.8)') 'x =', res_RF
            write(uf,'(a,i0)') 'Total number of iterations: ', totiter_RF
            !
            ! If the file was created, close it
            !
            if (media=='file' .or. media=='File') then
                write(*,*) 'Results written in data/output.dat'
                close(uf)
            end if
            !
            ! If output is desired to be written both in screen and file,
            ! repeat to write output (after writing to file) in screen
            !
            if (dummyvar==2) then
                uf = 6
                goto 10
            end if
            !
            return
        end subroutine write_output

end module
