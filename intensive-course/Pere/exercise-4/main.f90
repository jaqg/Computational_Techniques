! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Friday 15:33:02 20/01/2023 |
! +-------------------------------------------+

program power_method_Euclidean_scaling
    !
    ! Fortran program ot find the dominant eigenvalue and a unit dominant
    ! eigenvector for a symmetric nxn matrix
    !
    ! Modules
    !
    use io
    use mymodule
    use power_method
    !
    ! Variable definition
    !
    implicit none
    !

    !
    ! Formats
    !
    rfmt = 'f10.5'
    rfmt = trim(rfmt)
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+----------------------------------------+'
    write(*,*) '| Program power_method_Euclidean_scaling |'
    write(*,*) '+----------------------------------------+'
    write(*,*)
    !
    ! === READING INPUT ===
    !
    ! Read input parameters
    !
    open(newunit=ifu, file='data/input_parameters.dat', iostat=ios, &
    status="old", action="read", access="sequential")
    if (ios /= 0) stop "Error opening file data/input_parameters.dat"
    !
    read(ifu,*) 
    read(ifu,*) threshold
    !
    close(unit=ifu, iostat=ios)
    if (ios /= 0) stop "Error closing file unit ifu"
    !
    ! Read matrix from the input file
    !
    open(newunit=ifu, file='data/input_matrix.dat', iostat=ios, &
    status="old", action="read", access="sequential")
    if (ios /= 0) stop "Error opening file data/input_matrix.dat"
    !
    ! TODO
    !
    uf = 6
    !
    ! Close file
    !
    call read_sq_matrix(A, 1, ifu, '*')
    close(unit=ifu, iostat=ios)
    if (ios /= 0) stop "Error closing file unit ifu"
    !
    ! Check if the matrix is not symmetric
    !
    ! dcl1: do i = 1, size(A, dim=1)
    !     dcl2: do j = 1, size(A, dim=1)
    !         if (abs(A(i,j) - A(j,i)) > 0.0) then
    !             write(unit=6, fmt=*) 'ERROR: Matrix must be symmetric'
    !             stop
    !         end if
    !     end do dcl2
    ! end do dcl1 
    !
    ! Print input matrix
    !
    write(unit=uf, fmt='(A)') 'Input matrix:'
    call write_mat(A, uf, rfmt, size(A, dim=1))
    write(unit=uf, fmt=*)
    !
    ! Read arbitrary nonzero vector
    !
    open(newunit=ifu, file='data/input_vector.dat', iostat=ios, &
    status="old", action="read", access="sequential")
    if (ios /= 0) stop "Error opening file data/input_vector.dat"
    !
    call read_vec(v, 1, ifu, '*')
    !
    ! Close file
    !
    close(unit=ifu, iostat=ios)
    if (ios /= 0) stop "Error closing file unit ifu"
    !
    ! Print vector
    !
    write(unit=uf, fmt='(A)') 'Input vector:'
    call write_vec(v, uf, rfmt)
    write(unit=uf, fmt=*)
    !
    ! Power method Euclidean scalling
    !
    call PMES(A, v, threshold, vp, eval, niter)
    write(unit=uf, fmt='(A,f15.5)') 'Dominant eigenvalue: ', eval
    write(unit=uf, fmt=*)
    write(unit=uf, fmt='(A)') 'Dominant eigenvector: '
    write(unit=uf, fmt='(*(f15.5))') vp
    write(unit=uf, fmt=*)
    write(unit=uf, fmt='(A, i0)') 'Number of iterations: ', niter
    write(unit=uf, fmt=*)
    !
    stop
endprogram power_method_Euclidean_scaling
