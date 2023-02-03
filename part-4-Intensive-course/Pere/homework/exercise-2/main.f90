! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 10:18:14 25/01/2023 |
! +----------------------------------------------+

! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Wednesday 10:19:26 25-01-2023 |
! +-------------------------------------------+

program Huckel_Hamiltonian
    !
    ! 
    ! Modules
    !
    use io
    use mymodule
    use jacobi_method
    !
    ! Variable definition
    !
    implicit none
    !

    !
    ! Files
    !
    !
    ! Formats
    !
    !
    ! TODO: ask for file/screen to write the results
    !
    uf=6
    !
    ! === START OF THE PROGRAM ===
    !
    write(uf,*) '+----------------------------+'
    write(uf,*) '| Program Huckel_Hamiltonian |'
    write(uf,*) '+----------------------------+'
    write(uf,*)
    !
    ! READ INPUT
    !
    open(newunit=ifu, file='./data/input.dat', iostat=ios, status="old", action="read")
    if (ios /= 0) stop "Error opening file 'data/input.dat'"
    !
    read(ifu,*)
    read(ifu,*) tolerance
    !
    close(ifu)
    !
    ! TODO: change trivial matrix by Huckel matrix
    !
    dl1: do i = 1, 3
        dl2: do j = 1, 3
            A(i,j) = dble(i + j)
        end do dl2
    end do dl1
    !
    ! Print input
    !
    write(unit=uf, fmt='(A)') 'Input matrix:'
    call write_mat(A, uf, 'f10.5', size(A, dim=1)) 
    write(unit=uf, fmt=*)
    !
    write(*,*) 'Calculated eigenvalues: 0, 6 + sqrt(42), 6 - sqrt(42)'
    write(*,*)
    !
    write(unit=uf, fmt='(A,d10.2)') 'Tolerance for the Jacobi algorithm: ', &
    & tolerance
    write(unit=uf, fmt=*)
    !
    call jacobi(A, tolerance, v, ev)
    ! call jacobi(A, tolerance, ev)
    !
    stop
endprogram Huckel_Hamiltonian
