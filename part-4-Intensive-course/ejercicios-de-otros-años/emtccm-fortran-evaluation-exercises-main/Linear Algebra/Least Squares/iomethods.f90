! =========================================================================================
!                 Least Squares Method for Polynomials (Input Output Methods)
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

module IOmethods

    implicit none

    public :: ReadMat, WriteXY, ShowXY, WriteCs

    contains

    subroutine ReadMat(mat_fil_nam, my_mat)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to read matrices from files
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - mat_fil_nam: name of the file containing the matrix to be read
    !
    ! Outputs
    ! - my_mat: an array with the data from the matrix file
    ! Description
    ! The subroutine opens a file, counts the number of rows and, by detecting spaces, it
    ! counts the number of columns in the matrix. It then proceeds to read each row as a
    ! row of the matrix. It then closes the file.
    !
    ! -------------------------------------------------------------------------------------

        implicit none
        
        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        character(*), intent(in) :: mat_fil_nam
        real(kind=8), allocatable, intent(out) :: my_mat(:,:)
        
        ! Internal variables
        character(len=1000) :: leer, buf
        integer :: columnas, filas, stat, i, j
        
        ! Open the file
        open(40, file=mat_fil_nam, status='old')
        
        ! Read the first line of the file as text
        read(40, '(a)') leer
        
        ! Count the number of rows
        filas = 1
        do
            read(40, *, iostat=stat) buf
            if (stat < 0) exit
            filas = filas + 1
        end do
        
        ! Check for the number of columns by analyzing spaces in the first line of text
        do i = 1, len(trim(leer))
            if (i == 1) then
                if (leer(i:i) /= ' ') then ! If the first character is not blank ...
                    columnas = 1 ! ... then I have my first column!
                else
                    columnas = 0 ! If not, I still have no column.
                end if
            else ! If it's not the first charactar, it's not blank, but the previous one is ...
                if ((leer(i-1:i-1) == ' ') .and. (leer(i:i) /= ' ')) then
                    columnas = columnas + 1 ! ... I have a new column!
                end if
            end if
        end do

        ! Memory allocation for the matrix
        allocate(my_mat(columnas, filas))
        rewind(40)
        
        ! Read the matrix' entries
        read(40, *) ((my_mat(i,j), i=1, columnas), j=1, filas)
        
        ! Close the file
        close(40)

    end subroutine ReadMat

    subroutine WriteXY(f, C, R2, n, XY, eqns)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to write a CSV file with the coordinates to plot the fit
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f:         the polynomial function
    ! - C:         a vector with the coefficients for the function
    ! - R2:        the coefficient of determination
    ! - n:         the degree of the polynomial function
    ! - XY:        the original X and Y data points
    ! - eqns:      the number of (X, Y) entries in the original data
    !
    ! Outputs
    ! - in a file: with name 'new_data.csv', are the old (X,Y) values marked with 'o' and
    !              the new data for plotting marked with a '*'. It then runs a Python
    !              script which plots both sets of data.
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Importing the function ----------------------------------------------------------
        external f

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: n, eqns
        real(kind=8), intent(in) :: R2
        real(kind=8), allocatable, intent(in) :: C(:), XY(:,:)

        ! Internal variables --------------------------------------------------------------
        real(kind=8) :: f, mn, mx, domain, x, y
        character(20) :: detCoef, degree
        integer :: i, points

        mn = minval(XY(1,:)) - 0.5        ! Find the minimum value of x and subtracting 1/2
        mx = maxval(XY(1,:)) + 0.5        ! Find the maxumum value of x and adding 1/2

        domain = mx - mn                  ! Estimating the restricted domain of x

        points = int(domain * 40)         ! Computing 40 points per unit to be used


        open(40, file='new_data.csv', status='unknown' )    ! Opening file
        
        ! Writing o(x, y) to file
        do i = 1, eqns
            write(40, "(' ', A3, F14.8, ' ', F14.8)") "o", XY(1,i), XY(2,i)
        end do

        do i = 1, points
            x = mn + i*1.0/40                                   ! Computing new values of x
            y = f(C, x, n+1)                                    ! Computing new values of y
            write(40, "(' ', A3, F14.8, ' ', F14.8)") "*", x, y ! Writing *(x, y) to file
        end do
        
        close(40)                                               ! Closing file

        write(detCoef, "(F16.12)") R2
        write(degree, "(I5)") n
        call system('python plotter.py new_data.csv '//detCoef//degree)
    
    end subroutine WriteXY

    subroutine ShowXY(f, C, n, XY, eqns)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to show the new coordinates X and Y
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f:         the polynomial function
    ! - C:         a vector with the coefficients for the function
    ! - n:         the degree of the polynomial function
    ! - XY:        the original X and Y data points
    ! - eqns:      the number of (X, Y) entries in the original data
    !
    ! Outputs
    ! - on screen: 3 columns with the X, old Y and new (fitted) Y values
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Importing the function ----------------------------------------------------------
        external f

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: n, eqns
        real(kind=8), allocatable, intent(in) :: C(:), XY(:,:)

        ! Internal variables --------------------------------------------------------------
        real(kind=8) :: f, y
        integer :: i

        write(*, *) "|                  Old and fitted values                     |"
        write(*, *) '--------------------------------------------------------------'
        write(*, *) "|                   X                 Y        (fitted) Y    |"
        write(*, *) '--------------------------------------------------------------'

        do i = 1, eqns
            y = f(C, XY(1,i), n+1)
            write(*, "(' ', A, F18.10, F18.10, F18.10, A)") "|  ", XY(1,i), XY(2,i), y, "    |"
        end do

        write(*, *) '--------------------------------------------------------------'

    end subroutine ShowXY

    subroutine WriteCs(C, n)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to show, and write the coefficients in a CSV file
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - C:         a vector with the coefficients for the function
    ! - n:         the degree of the polynomial function
    !
    ! Outputs
    ! - in a file: with name 'coeffs.csv' are the numbered coefficients of the polynomial
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: n
        real(kind=8), allocatable, intent(in) :: C(:)

        ! Internal variables --------------------------------------------------------------
        integer :: i

        write(*, *) "|         Coefficients for the polynomial function           |"
        write(*, *) '--------------------------------------------------------------'

        open(40, file="coeffs.csv", status="unknown")
        do i = 1, n+1
            write(*, "(' ', A, I2, A, F18.10, A)") "|                c[", i-1, "]   = ", C(i), "                |"
            write(40, "(A5, I2, A3, F18.10)") "c[", i-1, "],  ", C(i)
        end do
        close(40)

        write(*, *) '--------------------------------------------------------------'
    
    end subroutine WriteCs

end module IOmethods