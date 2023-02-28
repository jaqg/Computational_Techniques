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
    character(len=80) :: fin_diff_method
    integer :: i, uf, ndim, maxiter, ierr, totiter
    real(kind=8) :: h, threshold
    real(kind=8), dimension(:), allocatable :: initcoord, mincoord, func
    real(kind=8), dimension(:,:), allocatable :: coord, grad, normgrad, conv
    !
    ! Subprograms
    !
    contains
    !
    subroutine read_input
        !
        ! Subroutine to read input
        !
        implicit none
        !
        integer :: nuf
        !
        open(newunit=nuf,file="data/input.dat",action="read",status='old')
        !
        ! Read number of dimensions (dependent variables)
        !
        read(nuf,*)
        read(nuf,*) ndim
        !
        ! Create a coordinates matrix for the initial coordinates
        !
        allocate(initcoord(ndim), stat=ierr)
        if (ierr .ne. 0) stop 'read_input: Error in allocation of initcoord'
        !
        ! Read value of initial points
        !
        read(nuf,*)
        read(nuf,*) initcoord(:)
        !
        ! Read value of threshold for convergence
        !
        read(nuf,*)
        read(nuf,*) threshold
        !
        ! Read method for the finite difference
        !
        read(nuf,*)
        read(nuf,*) fin_diff_method
        fin_diff_method = trim(fin_diff_method)
        !
        ! Read value of h for the finite difference
        !
        read(nuf,*)
        read(nuf,*) h
        !
        ! Close the file
        !
        close(nuf)
        !
        return
    end subroutine read_input
    
    subroutine print_output
        !
        ! Subroutine to print the output
        !
        implicit none
        !
        write(unit=uf, fmt=*)
        write(unit=uf, fmt='(a)') '=== RESULTS ==='
        write(unit=uf, fmt=*)
        !
        write(unit=uf, fmt='(2(4x,a),13x,a,12x,2(a,8x),a,7x,2(a,3x),a,7x,a)') &
        & 'it','x','y','f(x,y)','grad(x)','grad(y)','normgrad(x)', &
        & 'normgrad(y)', 'conv(x)', 'conv(y)'
        !
        wl1: do i = 1, totiter
            write(unit=uf, fmt='(i6,*(f14.8))') &
            & i, coord(i,1), coord(i,2), func(i), grad(i,1), grad(i,2), &
            & normgrad(i,1), normgrad(i,2), conv(i,1), conv(i,2)
        end do wl1
        !
        return
    end subroutine print_output 

end module
