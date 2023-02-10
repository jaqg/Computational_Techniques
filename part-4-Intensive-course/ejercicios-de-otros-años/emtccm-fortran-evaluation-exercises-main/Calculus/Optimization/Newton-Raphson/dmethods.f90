! =========================================================================================
!               Newton-Raphson Method for Optimization (Calculus Subroutines)
! -----------------------------------------------------------------------------------------
!
!     Author: Rony J. Letona
!     email:  rony.letona@estudiante.uam.es
!     Date:   March, 2022
!
! =========================================================================================

module dmethods

    implicit none

    ! Establishing the subroutines that will be provided to the external program
    public :: Hessian, Gradient, ShowHG

    contains

    subroutine Partial1(f, d, entry, coordsI, delta, P1)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to calculate the partial derivative of a function, using a
    ! finite centered method with respect to a single coordinate
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f:       the function
    ! - d:       the number of coordinates it uses
    ! - entry:   the coordinate to be used to derivate
    ! - coordsI: a vector with the coordinates to be used to evaluate the derivative
    ! - delta:   the dinite difference to be used to derivate numerically
    !
    ! Outputs
    ! - P1:      a real number with the value of the derivative on the given point with
    !            respect to the given coordinate
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Importing the function ----------------------------------------------------------
        external f

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: d, entry
        real(kind=8), intent(in) :: delta
        real(kind=8), allocatable, intent(in) :: coordsI(:)
        real(kind=8), intent(out) :: P1

        ! Internal variables --------------------------------------------------------------
        real(kind=8) :: f
        real(kind=8), allocatable :: active(:)

        ! Assigning memory to a dummy vector which will be used to select the coordinate
        allocate(active(d))
        ! Initialize the vector
        active = 0
        ! Activate the coordinate to be used to derivate
        active(entry) = delta

        ! Compute the derivative
        P1 = (f(coordsI + active, d) - f(coordsI - active, d)) / (2 * delta)

    end subroutine Partial1

    subroutine Partial2(f, d, entry1, entry2, coordsI, delta, P2)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to calculate the second derivative of a function, using a
    ! finite centered method with respect to two coordinates
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f:       the function
    ! - d:       the number of coordinates it uses
    ! - entry1:  the first coordinate to be used to derivate
    ! - entry2:  the second coordinate to be used to derivate
    ! - coordsI: a vector with the coordinates to be used to evaluate the derivative
    ! - delta:   the dinite difference to be used to derivate numerically
    !
    ! Outputs
    ! - P2:      a real number with the value of the second derivative on the given point
    !            with respect to the given coordinates
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Importing the function ----------------------------------------------------------
        external f

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: d, entry1, entry2
        real(kind=8), intent(in) :: delta
        real(kind=8), allocatable, intent(in) :: coordsI(:)
        real(kind=8), intent(out) :: P2

        ! Internal variables --------------------------------------------------------------
        real(kind=8) :: f
        real(kind=8), allocatable :: active1(:), active2(:)
        real(kind=8) :: n1, n2, n3, n4

        ! Assigning memory to dummy vectors which will be used to select the coordinate
        allocate(active1(d))
        allocate(active2(d))
        ! Initialize vectors
        active1 = 0
        active2 = 0
        ! Activating the coordinate to be used to derivate
        active1(entry1) = delta
        active2(entry2) = delta

        ! Compute all 4 terms of the numerator in the centered, finite mehtod
        n1 = f(coordsI + active1 + active2, d)
        n2 = f(coordsI - active1 + active2, d)
        n3 = f(coordsI + active1 - active2, d)
        n4 = f(coordsI - active1 - active2, d)

        ! Compute the second derivative
        P2 = (n1 - n2 - n3 + n4) / (4 * delta**2)

    end subroutine Partial2

    subroutine Gradient(f, d, coordsI, delta, G)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to calculate the Gradient of a function, using partial derivatives
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f:       the function
    ! - d:       the number of coordinates it uses
    ! - coordsI: a vector with the coordinates to be used to evaluate the derivative
    ! - delta:   the dinite difference to be used to derivate numerically
    !
    ! Outputs
    ! - G:       a vector with the partial derivative of the function over each coordinate
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Importing the function ----------------------------------------------------------
        external f

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: d
        real(kind=8), intent(in) :: delta
        real(kind=8), allocatable, intent(in) :: coordsI(:)
        real(kind=8), allocatable, intent(out) :: G(:)

        ! Internal variables --------------------------------------------------------------
        real(kind=8) :: f
        real(kind=8) :: temp
        integer :: i

        ! Assigning memory to the vector which will store the Gradient
        allocate(G(d))
        ! Initialize the vector
        G = 0
        ! Initialize the variable which will temporarily hold the partial derivative
        temp = 0

        do i = 1, d                                      ! Cycle through every coordinate
            call Partial1(f, d, i, coordsI, delta, temp) ! Partial derivative with respect
                                                         ! to i-th coordinate
            G(i) = temp                                  ! Store partial derivative
        end do

    end subroutine Gradient

    subroutine Hessian(f, d, coordsI, delta, H)
    ! -------------------------------------------------------------------------------------
    ! Special subroutine to calculate the Hessian of a function, using partial derivatives
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - f:       the function
    ! - d:       the number of coordinates it uses
    ! - coordsI: a vector with the coordinates to be used to evaluate the derivative
    ! - delta:   the dinite difference to be used to derivate numerically
    !
    ! Outputs
    ! - H:       a matrix with the partial second derivatives of the function over each
    !            pair of coordinates
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! Importing the function ----------------------------------------------------------
        external f

        ! In and out variables ------------------------------------------------------------
        integer, intent(in) :: d
        real(kind=8), intent(in) :: delta
        real(kind=8), allocatable, intent(in) :: coordsI(:)
        real(kind=8), allocatable, intent(out) :: H(:,:)

        ! Internal variables --------------------------------------------------------------
        real(kind=8) :: f
        real(kind=8) :: temp
        integer :: i, j

        ! Assigning memory to the matrix which will store the Hessian
        allocate(H(d,d))
        ! Initialize the matrix
        H = 0
        ! Initialize the variable which will temporarily hold the partial second derivative
        temp = 0

        do i = 1, d                                            ! First coordinate  - q[i]
            do j = 1, d                                        ! Second coordinate - q[j]
                call Partial2(f, d, i, j, coordsI, delta, temp)! d²/(di dj) F(q)
                H(i, j) = temp                                 ! Store partial 2. derivativ
            end do
        end do

    end subroutine Hessian

    subroutine Invert(mat, d, inv)
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

    end subroutine Invert

    subroutine ShowHG(hess, grad)
    ! -------------------------------------------------------------------------------------
    ! Subroutine to show the Hessian and the Gradient on screen
    ! -------------------------------------------------------------------------------------
    !
    ! Inputs
    ! - hess:    a matrix containing the hessian
    ! - grad:    a vector containing the gradient
    !
    ! Outputs
    ! - On screen: the provided hessian and gradient
    ! 
    ! -------------------------------------------------------------------------------------

        implicit none

        ! In and out variables ------------------------------------------------------------
        real(kind=8), allocatable, intent(in) :: hess(:,:), grad(:)

        ! Internal variables --------------------------------------------------------------
        integer :: d, i, j, f, c

        ! Find out the size of the hessian and the gradient
        c = size(hess, 1)
        f = size(hess, 2)
        d = size(grad)

        write(*, *) '--------------------------------------------------------------'
        write(*, "(' ', A)") "|               Hessian matrix over new coords               |"
        write(*, *) '--------------------------------------------------------------'
        write(*, *) '|                                                            |'

        do i=1, f
            write(*, "(' ', A)", advance='no') "|  "
            do j=1, c
                write(*, "(' ', F12.8)", advance='no') hess(j,i)
            end do
            write(*, *) ""
        end do

        write(*, *) '|                                                            |'
        write(*, *) '--------------------------------------------------------------'
        write(*, "(' ', A)") "|                  Gradient over new coords                  |"
        write(*, *) '--------------------------------------------------------------'
        write(*, *) '|                                                            |'

        do i = 1, d
            write(*, "(' ', A, E16.8, A)") "|                 ", grad(i), "                           |"
        end do

        write(*, *) '|                                                            |'
        write(*, *) '--------------------------------------------------------------'

    end subroutine ShowHG

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
                    M2(j,a) = M(j,i) ! Put the i-th row in the a-th row
                    M2(j,i) = M(j,a) ! And put the a-th row in the i-th row
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
    ! - M:    the matrix in question
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

        ! Carryy out Gauss algorithm to bring the matrix to upper-triangular form
        call Gauss(M)

        call ShowMat(M)

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

end module dmethods