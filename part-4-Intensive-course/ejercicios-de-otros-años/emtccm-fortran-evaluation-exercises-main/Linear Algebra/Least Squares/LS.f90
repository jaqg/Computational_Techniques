! =====================================================================================
!                       Least Squares Method for Polynomials
! -------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =====================================================================================

real(kind=8) function poly(C, x, d)
! -------------------------------------------------------------------------------------
! Function to compute the Y value of an n-th degree Polynomial
! -------------------------------------------------------------------------------------
!
! Inputs
! - C:    an array with all the polynomial's coefficients
! - x:    a real number to be evaluated in the function
! - d:    the degree of the polynomial function
!
! Outputs
! - poly: the value of an n-th degree polynomial evaluated on x
!
! -------------------------------------------------------------------------------------
        integer :: d, i
        real(kind=8) :: x, t
        real(kind=8), dimension(d) :: C
        t = 0
        do i = 1, d
            t = t + C(i) * x**(i - 1)
        end do
        poly = t
        return
    end function


program LeastSquares
! -----------------------------------------------------------------------------------------
! This program computes tries to fit a polynomial function to a collection of data points
! -----------------------------------------------------------------------------------------
!
! Inputs:
! - funcion:   A function to compute an n-th degree polynomial function. The 'n' has to be
!              provided by the user.
! - file:      A file with the data to be fitted.
!
! Outputs:
! - on screen: 
! - as a file: 
!
! -----------------------------------------------------------------------------------------

    ! Import the integration modules ------------------------------------------------------
    use LAmethods, only : AugMat, XMat, Inverse, ShowMat, R2Error
    use IOmethods, only : ReadMat, WriteXY, ShowXY, WriteCs

    implicit none

    ! Import the function defined at the beginning of the file ----------------------------
    external poly

    ! Variables ---------------------------------------------------------------------------
    integer :: col, row, i, j, n, clargs
    character(100) :: name_file, dummyDeg
    real(kind=8) :: poly, R2
    real(kind=8), allocatable :: XY(:,:), new_XY(:,:), aug_mat(:,:), proj_mat(:,:), inv_proj_mat(:,:)
    real(kind=8), allocatable :: coeffs(:), y_prime(:)

    ! Providing some GUI to the user ------------------------------------------------------
    write(*, *) '--------------------------------------------------------------'
    write(*, *) '|                 Polynomial Least Squares                   |'
    write(*, *) '--------------------------------------------------------------'
    write(*, *) '|                                                            |'
    write(*, *) '|  This program will attempt to fit a polynomial function    |'
    write(*, *) '|  to the provided data.                                     |'
    write(*, *) '|                                                            |'
    write(*, *) '|  You will have to enter the degree of the polynomial       |'
    write(*, *) "|  function you want to use. Keep in mind that:              |"
    write(*, *) "|                                                            |"
    write(*, *) '|                              2         3               n   |'
    write(*, *) "|   f(x) = c  + c  . x + c  . x  + c  . x  + ... + c  . x    |"            
    write(*, *) '|           0    1        2         3               n        |'
    write(*, *) '|                                                            |'
    write(*, *) "|  * Degree 1: a linear function (a straight line)           |"
    write(*, *) "|  * Degree 2: a quadratic function (a parabola)             |"
    write(*, *) "|  * Degree 3: a cubic function                              |"
    write(*, *) "|       .                                                    |"
    write(*, *) "|       .                                                    |"
    write(*, *) "|       .                                                    |"
    write(*, *) "|  * Degree n: an n-th dimensional polynomial function       |"
    write(*, *) "|                                                            |"
    write(*, *) "|  CAUTION: Please do not enter 0 or 'n', or the program     |"
    write(*, *) "|           not work. Moreover, make sure that the number    |"
    write(*, *) "|           you are entering is less than the number of      |"
    write(*, *) "|           equations (rows) in your input file.             |"
    write(*, *) "|                                                            |"

    ! Check if the user provided command line arguments
    clargs = COMMAND_ARGUMENT_COUNT()
    if (clargs == 2) then ! If so, extract the arguments from the file
        write(*, *) "|                                                            |"
        write(*, *) "|  Running in command line mode ...                          |"
        write(*, *) '|                                                            |'
        call GET_COMMAND_ARGUMENT(1,name_file)   ! Extract name of data file
        call GET_COMMAND_ARGUMENT(2,dummyDeg)    ! Extract degree of polynomial
        read(dummyDeg, *) n                      ! Parse the string into a real number

    ! If the user has no clue and failed to enter the correct command line arguments
    else if ((clargs > 2) .or. (clargs == 1)) then
        write(*, *) "|                                                            |"
        write(*, *) "|  WARNING: Input error                                      |"
        write(*, *) "|                                                            |"
        write(*, *) "|  To use command line arguments, please use the following   |"
        write(*, *) "|  format:                                                   |"
        write(*, *) "|                                                            |"
        write(*, *) "|  $ ./LS.exe  name_of_file.dat  degree                      |"
        write(*, *) "|                                                            |"
        write(*, *) "|                                                            |"
        write(*, *) "|  ./LS.exe      : the program to be run                     |"
        write(*, *) "|                                                            |"
        write(*, *) "|  degree            : the degree of the polynomial          |"
        write(*, *) "|                                                            |"
        write(*, *) "|  name_of_file.dat  : the name of the data file             |"
        write(*, *) "|                                                            |"
        write(*, *) "|                                                            |"
        write(*, *) "|  Please try again, with the correct format.   Exiting ...  |"
        write(*, *) "|                                                            |"
        write(*, *) '--------------------------------------------------------------'

    ! If the user provided no command line arguments, run in interactive mode
    else
        write(*, *) "|                                                            |"
        write(*, *) "|  Running in interactive mode ...                           |"
        write(*, *) '|                                                            |'
        write(*, *) '|                                                            |'
        write(*, "(' ', A)", advance='no') '|  Please enter the name of the data file: '
        read(*, *) name_file
        write(*, *) '|                                                            |'
        write(*, "(' ', A)", advance='no') '|  Please enter the degree of the polynomial: '
        read(*, *) n
    end if

    write(*, *) '|                                                            |'
    write(*, *) '|    Accepted value:                                         |'
    write(*, *) '|                                                            |'
    write(*, "(' ', A29, I4, A29)") "|    Degree of polynomial =  ",n,"                            |"
    write(*, *) '|                                                            |'
    write(*, *) '|                         Computing ...                      |'
    write(*, *) '|                                                            |'
    write(*, *) '--------------------------------------------------------------'

    ! Read the matrix from the selected file.
    call ReadMat(trim(name_file), XY)

    col = size(XY, 1)  ! Extract the number of columns
    row = size(XY, 2)  ! Extract the number of rows

    ! If the number of columns is different than 2,
    ! or the user is providing less equations than
    ! variables, quit the program.
    if ((col /= 2) .or. (row <= n)) then
        write(*, *) '|                                                            |'
        write(*, *) "| The provided data does not have the correct dimensions for |"
        write(*, *) "| a least squares calculation.                               |"
        write(*, *) '|                                                            |'
        write(*, *) "| Dimensions should be:                                      |"
        write(*, *) '|                                                            |'
        write(*, "(' ', A, I3, A)") "|     * Rows > ", n, "   pairs of coordinates                    |"
        write(*, *) "|     * Columns = 2  (x, y) values                           |"
        write(*, *) '|                                                            |'
        write(*, *) "| Please check your input.          Terminating program.     |"
        write(*, *) '|                                                            |'
        write(*, *) '--------------------------------------------------------------'
        stop
    end if

    ! Assign memory to the matrices and vectors
    allocate(proj_mat(row+1, row+1))
    allocate(coeffs(n+1))

    ! Compute the matrix X where every column is a power of x, and every
    ! row a value of x
    call XMat(XY, row, n, aug_mat)

    ! Compute the new vector z resulting from X^t.y
    ! where X^t is the transpose of the preivou
    ! matrix and y is the vector with the old y values
    y_prime = matmul(aug_mat, XY(2,:))

    ! Compute the matrix B resulting from the multiplication of X.X^t
    ! Since we couldn't do it the easy way (we were asked for sums),
    ! I did it with do-loops.
    ! The alternative solution is also explaned.
    call AugMat(XY, row, n, proj_mat)

    ! Compute the inverse of B, B^(-1), which is the same as (X.X^t)^-1
    ! This procedure is carried out via the Gauss-Jordan algorithm
    call Inverse(proj_mat, n+1, inv_proj_mat)

    ! Finally, compute B^(-1).z in order to obtain the vector with the
    ! coefficients for the selected polynomial
    coeffs = matmul(inv_proj_mat, y_prime)

    ! Compute the error as the coefficient of determination R²
    call R2Error(poly, XY, row, coeffs, n, R2)

    ! Create a new file with the old coordinates, and new
    ! coordinates for plotting. This subroutine also calls an external
    ! Python script to do the plotting.
    call WriteXY(poly, coeffs, R2, n, XY, row)

    write(*, *) '--------------------------------------------------------------'
    write(*, *) '|                                                            |'
    write(*, *) "|                     Fitting completed!                     |"
    write(*, *) '|                                                            |'
    write(*, *) '--------------------------------------------------------------'
    write(*, *) "|                Coefficient of determination                |"
    write(*, *) '--------------------------------------------------------------'
    write(*, "(' ', A, F14.12, A)") "|                   R²  =  ",R2,"                    |"
    write(*, *) '--------------------------------------------------------------'

    ! Display the newly fownd coefficients on screen.
    call WriteCs(coeffs, n)

    ! Show the old coordinates (x, y) and the new, fitted,
    ! coordinates y' for the user to be able to compare.
    call ShowXY(poly, coeffs, n, XY, row)

end program LeastSquares