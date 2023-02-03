! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 10:51:54 25/01/2023 |
! +----------------------------------------------+

module jacobi_method
    !
    ! 
    !
    implicit none
    !
    contains
    !
    subroutine pi_numb(pi)
        !
        ! Subroutine to compute pi number with the Machin's formula
        !
        implicit none
        real(kind=8), intent(out) :: pi
        !
        pi = 4.0_8 * (4.0_8 * datan(1.0_8/5.0_8) - datan(1.0_8/239.0_8))
        !
        return
    end subroutine pi_numb 
    !
    subroutine transpose_mat(A, B)
        implicit none
        real(kind=8), dimension(:,:), intent(in) :: A
        real(kind=8), dimension(:,:), allocatable, intent(out) :: B
        !
        integer :: i, j, n, m, ierr
        !
        n = size(A, dim=1)
        m = size(A, dim=2)
        !
        allocate(B(m,n), stat=ierr)
        if (ierr .ne. 0) then
            write(*,*) 'jacobi_method.f90 transpose_mat: Error in '
            write(*,*) 'allocation of B'
            stop
        end if
        !
        dl1: do i = 1, m
            dl2: do j = 1, n
                B(i,j) = A(j,i)
            end do dl2
        end do dl1
        !
        return
    end subroutine transpose_mat 
    !
    subroutine jacobi(A, tolerance, v, ev)
        !
        ! 
        !
        implicit none
        !
        real(kind=8), dimension(:,:), intent(in) :: A
        real(kind=8), intent(in) :: tolerance
        real(kind=8), dimension(:), allocatable, intent(out) :: v
        real(kind=8), intent(out) :: ev
        !
        ! Dummy variables
        !
        integer :: i, j, n, m, ierr, row, col
        integer, dimension(2) :: maxelem
        real(kind=8) :: pi, aii, aik, aki, akk, R, sigma, dii, dkk, phi
        real(kind=8), dimension(:,:), allocatable :: O, OT, D, Dtmp
        !
        call pi_numb(pi)
        !
        n = size(A, dim=1)
        m = size(A, dim=2)
        !
        if (n /= m) then
            write(unit=6, fmt=*) 'ERROR jacobi_method.f90 jacobi: Input matrix'
            write(unit=6, fmt=*) 'is not a square matrix'
            stop
        end if
        !
        allocate(v(n), stat=ierr)
        if (ierr .ne. 0) stop 'main.f90: Error in allocation of v'
        !
        ev = 0.0_8
        !
        ! dl1: do i = 1, n
        !     dl2: do j = 1, m
        !         !
        !         if (abs(A(i,j)) > tolerance) then
        !             write(unit=6, fmt=*) 'Yes'
        !         end if
        !         !
        !     end do dl2
        ! end do dl1
        !
        maxelem = maxloc( abs(A) )
        !
        row = maxelem(1)
        col = maxelem(2)
        !
        aii = A( row, row )
        aik = A( row, col )
        aki = A( col, row )
        akk = A( col, col )
        !
        R = dsqrt( (aii - akk)**2 + 4.0_8 * aik**2 )
        !
        if (aii >= akk) then
            sigma = 1.0_8
        else
            sigma = -1.0_8
        end if
        ! 
        ! dii = 1.0_8/2.0_8 * (aii + akk + sigma * R)
        ! dkk = 1.0_8/2.0_8 * (aii + akk - sigma * R)
        ! dik = 0.0_8
        ! dki = dik
        ! !
        ! D( row, row ) = dii
        ! D( row, col ) = dik
        ! D( col, row ) = dki
        ! D( col, col ) = dkk
        !
        ! write(*,*) 'ev1 =', dii
        ! write(*,*) 'ev2 =', dkk
        !
        ! Compute phi
        !
        ic1: if (abs(aii - akk) < 1.D-15) then
            ic2: if (aik > 0) then
                phi = pi/4.0_8
            else
                phi = -pi/4.0_8
            end if ic2
        else
            phi = 1.0_8/2.0_8 * datan( 2.0_8 * aik/(aii - akk) )
        end if ic1
        !
        ! Compute matrix O
        !
        allocate(O(n,n), stat=ierr)
        if (ierr .ne. 0) stop 'jacobi_method.f90 jacobi: Error in allocation of O'
        !
        ! Set zeros to all non-diagonal elements and -temporally- ones to diag
        !
        O = 0.0_8
        !
        dlo1: do i = 1, n
            O(i,i) = 1.0_8
        end do dlo1
        !
        ! Change elements of 2x2 rotation minor
        !
        O(row,row) = dcos(phi)
        O(row,col) = -dsin(phi)
        O(col,row) = -O(row,col)
        O(col,col) = O(row,row)
        !
        write(*,*) 'Matrix O'
        do i = 1, n
            write(*,*) ( O(i,j), j=1, n )
        end do 
        write(*,*)
        !
        ! Apply the rotation: O^T A O = D
        !
        call transpose_mat(O, OT)
        !
        allocate(Dtmp(n,n), stat=ierr)
        if (ierr .ne. 0) stop 'jacobi_method.f90 jacobi: Error in allocation of Dtmp'
        !
        allocate(D(n,n), stat=ierr)
        if (ierr .ne. 0) stop 'jacobi_method.f90 jacobi: Error in allocation of D'
        !
        Dtmp = MATMUL(OT, A)
        D = MATMUL(Dtmp, O)
        !
        write(*,*) 'Matrix D'
        do i = 1, n
            write(*,*) ( D(i,j), j=1, n )
        end do 
        !
        return
    end subroutine jacobi     
    !
end module
