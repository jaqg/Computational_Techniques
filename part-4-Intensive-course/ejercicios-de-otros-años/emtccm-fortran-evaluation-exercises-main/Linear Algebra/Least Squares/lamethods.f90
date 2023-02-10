! =========================================================================================
!             Least Squares Method for Polynomials (Linear Algebra Subroutines)
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

module LAmethods

    implicit none

    public :: XMat, AugMat, Inverse, ShowMat, R2Error

    contains

    subroutine XMat(XY, eqns, n, new_mat)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to compute the matrix X where every column is a power of x, and
    ! every row a value of x
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - XY:   an array of real values of x and y (the raw data)
    ! - eqns: the number of data points (or equations to be solved)
    ! - n:    the degree of the polinomial (the second dimension of the matrix)
    !
    ! Outputs
    ! - new_mat: an array with the computed values of x for each power in the polynomial
    !
    ! -------------------------------------------------------------------------------------

        implicit none
        
        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        integer, intent(in) :: n, eqns
        real(kind=8), allocatable, intent(in) :: XY(:,:)
        real(kind=8), allocatable, intent(out) :: new_mat(:,:)
        
        ! Internal variables
        real(kind=8) :: temp
        integer :: i, j, l
        real(kind=8), allocatable :: temp_mat(:,:), inv_temp_mat(:,:)

        ! Assigning memory to the output matrix
        allocate(new_mat(n+1, eqns))
        allocate(inv_temp_mat(eqns, n+1))

        do i = 1, n+1
            do j = 1, eqns
                new_mat(i, j) = XY(1, j)**(i-1)
            end do
        end do

    end subroutine XMat

    subroutine AugMat(XY, eqns, n, new_mat)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to compute the projection matrix B = X.X^t
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - XY:   an array of real values of X and Y (the raw data)
    ! - eqns: the number of data points (or equations to be solved)
    ! - n:    the degree of the polinomial (the second dimension of the matrix)
    !
    ! Outputs
    ! - new_mat: an array with the computed values of X for each power in the polynomial
    !
    ! -------------------------------------------------------------------------------------

        implicit none
        
        ! Defining variables --------------------------------------------------------------
        
        ! In and out variables
        integer, intent(in) :: n, eqns
        real(kind=8), allocatable, intent(in) :: XY(:,:)
        real(kind=8), allocatable, intent(out) :: new_mat(:,:)
        
        ! Internal variables
        
        ! The following line can be used if I coded this the easy way (read Dilemma)
        ! real(kind=8), allocatable :: aug_mat(:,:)

        ! These are the variables necessary to do it the hard way
        real(kind=8) :: temp
        integer :: i, j, l

        ! Assigning memory to the output matrix
        allocate(new_mat(n+1, n+1))

        ! Dilemma -------------------------------------------------------------------------
        ! There are two ways of doing this:
        ! (a) The easy and legible way
        ! (b) The hard way with loops

        ! The easy way looks like this <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        
        ! call XMat(XY, eqns, n, aug_mat)                 ! I could even pass 'aug_mat' to
                                                        ! this subroutine, since I have it
                                                        ! in the main program already.
        ! new_mat = matmul(aug_mat, transpose(aug_mat))

        ! And that would be it <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

        ! However, since the homework asked us to compute all the sums, I coded the hard
        ! way as well. And, in fact, it's the code being used.

        ! The hard way looks like this >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        do i = 1, n + 1                                 ! Go over all the columns
            do j = i, n + 1                             ! Go over the rows starting at the
                                                        ! number of column (only half)
                temp = 0

                do l = 1, eqns                          ! Compute the sum over all powers
                    temp = temp + XY(1, l)**(i + j - 2) ! of x
                end do
                
                new_mat(i, j) = temp                    ! Save this value in the out matrix
                
                if (i /= j) then                        ! If entry not in the diagonal
                    new_mat(j, i) = temp                ! Save value in the 'transposed'
                end if                                  ! position of the matrix
            
            end do
        end do
        ! And after 3 loops, there it is >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    end subroutine AugMat

    subroutine AddRow(M, a, b, d, M2)
    ! -------------------------------------------------------------------------------------
    ! Subroutine to add one row of a matrix to another row in the same matrix
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - M:    the matrix in question
    ! - a:    the destination row
    ! - b:    the source row
    ! - d:    the number of elements in the rows
    !
    ! Outputs
    ! - M2:   the output matrix with the added row
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: a, b, d
        real(kind=8), allocatable, intent(in) :: M(:,:)
        real(kind=8), allocatable, intent(out) :: M2(:,:)

        ! Internal variables --------------------------------------------------------------
        integer :: i

        ! Assigning memory to the output matrix
        allocate(M2(d,d))

        ! Copying all data from matrix M to M2
        M2 = M

        ! Making the a-th row in M2, equal to the sum of the a-th and b-th rows in M
        do i=1, d
            M2(i,a) = M(i,a) + M(i,b)
        end do

    end subroutine AddRow

    subroutine MultiRow(M, f, c, n, d, M2)
    ! -------------------------------------------------------------------------------------
    ! Subroutine to multiply one row of a matrix by a constant
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - M:    the matrix in question
    ! - f:    the row that will be multiplied by a constant
    ! - c:    the column where the multiplication should start from
    ! - n:    the constant
    ! - d:    the number of elements in the rows
    !
    ! Outputs
    ! - M2:   the output matrix with the added row
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: f, c, d
        real(kind=8), intent(in) :: n
        real(kind=8), allocatable, intent(in) :: M(:,:)
        real(kind=8), allocatable, intent(out) :: M2(:,:)

        ! Internal variables --------------------------------------------------------------
        integer :: i

        ! Assigning memory to the output matrix
        allocate(M2(d,d))

        ! Copying all data from matrix M to M2
        M2 = M

        ! Multiplying the f-th row of the matrix by the constant n, starting at column c
        do i=c, d
            M2(i,f) = M(i,f) * n
        end do
        
    end subroutine MultiRow

    subroutine SwapFinder(M, a, d, M2)
    ! -------------------------------------------------------------------------------------
    ! Subroutine to check whether a diagonal element of the matrix is 0, if so, swap lower
    ! rows until it's not 0
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - M:    the matrix in question
    ! - a:    the row that will be multiplied by a constant
    ! - d:    the number of elements in the rows
    !
    ! Outputs
    ! - M2:   the output matrix with the added row
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: a, d
        real(kind=8), allocatable, intent(in) :: M(:,:)
        real(kind=8), allocatable, intent(out) :: M2(:,:)

        ! Internal variables --------------------------------------------------------------
        integer :: i, j

        ! Assigning memory to the output matrix
        allocate(M2(d,d))

        ! Copying all data from matrix M to M2
        M2 = M

        do i=a, d                    ! Beginning with the provided row
            if (M(i,a) .ne. 0) then  ! If the a-th element in the i-th row is not 0
                do j=1, d
                    M2(:,a) = M(:,i) ! Put the i-th row in the a-th row
                    M2(:,i) = M(:,a) ! And put the a-th row in the i-th row
                end do
                exit
            end if
        end do

    end subroutine SwapFinder

    subroutine Gauss(M)
    ! -------------------------------------------------------------------------------------
    ! Subroutine to carry out the Gauss algorithm to convert a matrix into upper-triangular
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - M:   the matrix in question
    !
    ! Outputs
    ! - M:   the same matrix in upper-triangular form
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        real(kind=8), allocatable, intent(inout) :: M(:,:)

        ! Internal variables --------------------------------------------------------------
        real(kind=8), allocatable :: M2(:,:)
        integer :: i, j, col, fil

        ! Extracting the number of rows and colums of the matrix
        col = size(M, 1) ! Columns
        fil = size(M, 2) ! Rows

        ! Assigning memory to the dummy matrix
        allocate(M2(col,fil))

        do i=1, fil                                           ! Let's go row by row ...

            ! The idea here is to be sure that the diagonal element is equal to 1
            if (M(i,i) == 0) then                             ! In case the value is 0 ...
                call SwapFinder(M, i, fil, M2)                ! .. swap rows until it's not
                call MultiRow(M2, i, i, 1/M(i,i), col, M2)    ! Multiply entire row to make
                                                              ! that value equal to 1
            else                                              ! If the value was not 0 ...
                call MultiRow(M, i, i, 1/M(i,i), col, M2)     ! ... multiply the row and
                                                              ! make the value equal to 1
            end if

            M = M2                                            ! Copy the new matrix M2 to
                                                              ! the original one M.

            ! The idea here is to make all subsequent elements below the diagonal equal
            ! to zero
            do j=(i+1), fil                                   ! From the next row ...
                if ( abs(M(i,j)) > 1D-9 ) then                ! *If the value is big enough
                    call MultiRow(M, j, i, -1/M(i,j), col, M2)! Make element of row -1
                    M = M2                                    ! Copy new matrix to original
                    call AddRow(M, j, i, col, M2)             ! Add previous row to this
                    M = M2                                    ! Copy new matrix to original
                    if (abs(M(i,j)) < 1D-9) then              ! *If the value is not big
                        M(i,j) = 0.0                          ! Make the value 0
                    end if
                else                                          ! *If the value is not big
                    M(i,j) = 0                                ! Make the value 0
                end if
            end do
            M = M2                                            ! Copy new matrix to original
        end do

    end subroutine Gauss

    subroutine Jordan(M)
    ! -------------------------------------------------------------------------------------
    ! Subroutine to carry out the Gauss-Jordan algorithm to convert a matrix into its
    ! reduced row echelon form
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - M:    the matrix in question
    !
    ! Outputs
    ! - M:   the same matrix in reduced row echelon form
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        real(kind=8), allocatable, intent(inout) :: M(:,:)

        ! Internal variables --------------------------------------------------------------
        integer :: i, j, col, fil

        ! Extracting the number of rows and colums of the matrix
        col = size(M, 1)
        fil = size(M, 2)

        ! Carry out Gauss algorithm to bring the matrix to upper-triangular form
        call Gauss(M)

        do i=fil, 1, -1                                       ! Go row by row backwards
            do j=i-1, 1, -1                                   ! From the next row to the 1.
                if ( abs(M(i,j)) > 1D-9 ) then                ! Is the value big enough?
                    ! Subtract the pivot row multiplied by this row's negative value
                    M(1:col,j) = M(1:col,j) - M(i,j) * M(1:col,i)
                else                                          ! If the value is not big ...
                    M(i,j) = 0                                ! ... make it 0.
                end if
            end do
        end do

    end subroutine Jordan

    subroutine Inverse(mat, d, inv)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to invert a matrix using the Gauss-Jordan algorithm
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - mat:     the original matrix to be inverted
    ! - d:       the number of coordinates it uses
    !
    ! Outputs
    ! - inv:     the inverted matrix
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: d
        real(kind=8), allocatable, intent(in) :: mat(:,:)
        real(kind=8), allocatable, intent(out) :: inv(:,:)

        ! Internal variables --------------------------------------------------------------
        real(kind=8), allocatable :: GJ(:,:)
        integer :: i, j

        ! Assigning memory to a expanded dummy matrix (twice the number of columns)
        allocate(GJ(d*2, d))
        ! Assigning memory to the inverted matrix
        allocate(inv(d,d))
        ! Initialize the expanded dummy matrix
        GJ = 0
        ! Initialize the inverse matrix
        inv = 0

        do i = 1, d
            do j = 1, d
                GJ(i,j) = mat(i,j)     ! The first half of dummy is equal to the original
            end do
            GJ(i+d, i) = 1             ! The second half of dummy is the identity matrix
        end do

        call Jordan(GJ)                ! Run the Gauss-Jordan algorithm over dummy matrix

        do i = 1, d
            do j = 1, d
                inv(i,j) = GJ(i+d, j)  ! The inverse is equal to the second half of dummy
            end do
        end do

    end subroutine Inverse

    subroutine ShowMat(M)
    ! -------------------------------------------------------------------------------------
    ! Helper subroutine to help me see the contents of any matrix when testing
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - M:  Matrix whose values I will see
    !
    ! Outputs
    ! - on screen: the matrix in its natural representation
    !
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Defining variables --------------------------------------------------------------
        ! In and out variables
        real(kind=8), allocatable, intent(in) :: M(:,:)

        ! Internal variables
        integer :: i, j, f, c

        ! Get the number of columns and rows
        c = size(M, 1)
        f = size(M, 2)

        write(*, *) '--------------------------------------------------------------'
        do i=1, f
            do j=1, c
                write(*, "(' ', F12.5)", advance='no') M(j,i)
            end do
            write(*, *) ""
        end do
        write(*, *) '--------------------------------------------------------------'

    end subroutine ShowMat

    subroutine R2Error(f, XY, eqns, C, n, R2)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to compute the determination coefficient
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f:       the polynomial function to evaluate every x and calculate every y given C 
    ! - XY:      an array of real values of x and y (the raw data)
    ! - eqns:    the number of data points
    ! - C:       an array with the coefficients for the function
    ! - n:       the degree of the polynomial function
    !
    ! Outputs
    ! - R2:      the determination coefficient
    ! 
    ! -------------------------------------------------------------------------------------
        
        implicit none

        ! Importing the function ----------------------------------------------------------
        external f

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: eqns
        real(kind=8), allocatable, intent(in) :: C(:), XY(:,:)
        real(kind=8), intent(out) :: R2

        ! Internal variables --------------------------------------------------------------
        real(kind=8) :: f
        real(kind=8) :: SS_res, SS_tot, mean_y
        integer :: i, n

        ! Computing the mean value of the y data points
        mean_y = sum(XY(2,:)) / eqns

        ! Initializing variables
        SS_res = 0                  ! Residual Sum of Squares
        SS_tot = 0                  ! Total Sum of Squares

        do i = 1, eqns
            SS_res = SS_res + ( XY(2, i) - f(C, XY(1, i), n+1) )**2 ! Residual error squared
            SS_tot = SS_tot + ( XY(2, i) - mean_y )**2              ! Mean error squared
        end do

        R2 = 1 - SS_res/SS_tot      ! Coefficient of Determination

    end subroutine R2Error

end module LAmethods