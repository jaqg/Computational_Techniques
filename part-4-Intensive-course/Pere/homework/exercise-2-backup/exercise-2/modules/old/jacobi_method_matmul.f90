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
    subroutine is_sq_mat(A, ans)
        implicit none
        real(kind=8), dimension(:,:), intent(in) :: A
        logical, intent(out) :: ans
        !
        if (size(A, dim=1) /= size(A, dim=2)) then
            ans=.false. 
        else
            ans=.true.
        end if
        !
        return
    end subroutine is_sq_mat 
    !
    subroutine identity_mat(A, B)
        implicit none
        real(kind=8), dimension(:,:), intent(in) :: A
        real(kind=8), dimension(:,:), allocatable, intent(out) :: B
        ! Dummy variables
        integer :: i, n, m, ierr 
        logical :: ans
        !
        ! Check if the matrix is squared
        !
        call is_sq_mat(A,ans)
        !
        if (ans .eqv. .false.) stop &
            &'jacobi_method.f90 identity_mat: matrix not squared'
        !
        n = size(A, dim=1)
        m = size(A, dim=2)
        !
        ! Allocate output (identity) matrix
        !
        allocate(B(n, m), stat=ierr)
        if (ierr .ne. 0) stop &
            &'jacobi_method.f90 identity_mat: Error in allocation of B'
        !
        B = 0.0_8
        !
        ! Only assign to 1 the diagonal elements
        !
        dl1: do i = 1, n
            B(i,i) = 1.0_8
        end do dl1
        !
        return
    end subroutine identity_mat 
    !
    subroutine phi_plane_rot(app,apq,aqp,aqq, phi)
        implicit none
        real(kind=8), intent(in) :: app,apq,aqp,aqq
        real(kind=8), intent(out) :: phi
        ! Dummy variables
        real(kind=8) :: pi 
        !
        ! Compute pi number
        !
        call pi_numb(pi)
        !
        ! Compute phi:
        !
        !         -
        !        | 1/2 arctg(2*apq/(aqq - app))              if app /= aqq
        ! phi = -
        !        | pi/4 when apq > 0 ; -pi/4 when apq < 0    if app = aqq
        !         -
        !
        ! If app = aqq, phi = 1/2 * arctg(+-inf). As tan(pi/2) -> inf, then
        ! arctg(+-inf) = +-pi/2, and phi = +-pi/4
        !
        ! As app and aqq are floating numbers, the equality app = aqq cannot
        ! be performed; therefore, the condition |aqq - app| < small number
        ! is imposed
        !
        ic1: if (abs(aqq - app) < 1.D-15) then
            ic2: if (apq > 0) then
                phi = pi/4.0_8
            elseif (apq < 0) then
                phi = -pi/4.0_8
            end if ic2
        else
            phi = 1.0_8/2.0_8 * datan( 2.0_8 * apq/(aqq - app) )
            ! phi = 1.0_8/2.0_8 * datan( 2.0_8 * apq/(app - aqq) )
        end if ic1
        !
        return
    end subroutine phi_plane_rot
    !
    subroutine abs_max_elems(A, p, q, app, apq, aqp, aqq)
        implicit none
        real(kind=8), dimension(:,:), intent(in) :: A
        integer, intent(out) :: p, q
        real(kind=8), intent(out) :: app, apq, aqp, aqq
        ! Dummy variables
        integer :: i, j, n
        real(kind=8) :: max_val 
        !
        ! Initialize variables
        !
        n = size(A, dim=1)
        max_val = 0.0_8
        !
        ! Get the -absolute value- max. non-diagonal element of A
        ! Since A must be symmetric, only the lower triangular non-diagonal
        ! elements are maped
        !
        do j= 1, n-1
            do i = j + 1, n
                if (abs(A(i,j)) > abs(max_val)) then
                    !
                    ! And store the element position as A(p,q)
                    !
                    p = i
                    q = j
                    !
                    ! Update max_val
                    !
                    max_val = A(i,j)
                    !
                else
                end if
            end do
        end do
        !
        ! Get the elements app, aqq, apq, aqp
        !
        app = A(p,p)
        aqq = A(q,q)
        apq = A(p,q)
        aqp = A(q,p)
        !
        return
    end subroutine abs_max_elems
    !
    subroutine jacobi(A, threshold, v, ev)
        !
        ! 
        !
        implicit none
        !
        real(kind=8), dimension(:,:) :: A
        real(kind=8), intent(in) :: threshold
        real(kind=8), dimension(:,:), allocatable, intent(out) :: v
        real(kind=8), dimension(:), allocatable, intent(out) :: ev
        !
        ! Dummy variables
        !
        integer :: i, j, totiter, n, m, ierr, p, q, r
        logical :: ans
        real(kind=8) :: app, apq, aqp, aqq, phi, c, s
        real(kind=8), dimension(:,:), allocatable :: Ppq, PpqT, A_aux
        ! real(kind=8), dimension(:,:), allocatable :: O, OT, D, Dtmp
        !
        ! Check if the input matrix is squared
        !
        call is_sq_mat(A,ans)
        !
        ! In case it is not, stop
        !
        if (ans .eqv. .false.) then
            write(unit=6, fmt=*) 'ERROR jacobi_method.f90 jacobi: Input matrix'
            write(unit=6, fmt=*) 'is not a square matrix'
            stop
        end if
        !
        ! If it is squared, store the dimensionality n
        !
        n = size(A, dim=1)
        !
        ! Allocate arrays
        !
        allocate(v(n,n), stat=ierr)
        if (ierr .ne. 0) stop &
        & 'ERROR jacobi_method.f90 jacobi: Error in allocation of v'
        !
        allocate(ev(n), stat=ierr)
        if (ierr .ne. 0) stop &
        & 'ERROR jacobi_method.f90 jacobi: Error in allocation of ev'
        !
        ! Initialize variables: 
        !   Eigenvector matrix V as the identity matrix
        !   Eigenvalues list as zeroes
        !   Total number of iterations of the method
        !
        call identity_mat(v, v)
        ev = 0.0_8
        totiter = 0
        !
        ! dwv1: do i = 1, n
        !     write(*,'(*(f4.1))') ( v(i,j), j=1, n )
        ! end do dwv1
        !
        !
        ! Main loop: iterate until the convergence condition is fulfilled
        !
        ml: do
            !
            ! Update number of iterations
            !
            totiter = totiter + 1
            !
            ! Get the -absolute value- max. element of A
            !
            call abs_max_elems(A, p, q, app, apq, aqp, aqq)
            !
            ! write(*,*) 'app =', app
            ! write(*,*) 'apq =', apq
            ! write(*,*) 'aqp =', aqp
            ! write(*,*) 'aqq =', aqq
            !
            ! Check for convergence: check if the largest (absolute value) non-
            ! diagonal element is below the threshold
            !
            if (abs(apq) < abs(threshold)) exit ml 
            !
            write(*,'(a,i0,a)') '--- ITERATION ', totiter, ' ---'
            write(*,*)
            !
            write(*,'("abs_max_elems(A): A(",i0,",",i0,") =", f10.5)') p, q, A(p,q)
            !
            ! Compute angle phi for the plane rotation
            !
            call phi_plane_rot(app,apq,aqp,aqq, phi)
            !
            write(*,*) 'phi =', phi
            write(*,*) 
            !
            ! Apply the rotation: A' = P^T * A * P -> update altered elements of A
            ! only elements of A in rows and columns p,q are altered as:
            !                                _
            !   a'_{rp} = c a_{rp} - s a_{rq} |
            !                                  - For r/=p, r/=q
            !   a'_{rq} = c a_{rq} + s a_{rp} |
            !                                -
            !   a'_{pp} = c^2 a_{pp} + s^2 a_{qq} - 2 s c a_{pq}
            !   a'_{qq} = c^2 a_{qq} + s^2 a_{pp} + 2 s c a_{pq}
            !   a'_{pq} = (c^2 - s^2) a_{pq} + s c (a_{pp} - a_{qq})
            !
            ! where
            !
            !   s = sin(phi) ; c = cos(phi)
            !
            s = dsin(phi)
            c = dcos(phi)
            !
            ! A(p,p) = c**2 * app + s**2 * aqq - 2.0_8 * s * c * apq
            ! A(q,q) = c**2 * aqq + s**2 * app + 2.0_8 * s * c * apq
            ! A(p,q) = (c**2 - s**2) * apq + s * c * (app - aqq)
            ! A(q,p) = A(p,q)
            ! !
            ! dl1: do r = 1, n
            !     if (r /= p .and. r /= q) then
            !         A(r,p) = c * A(r,p) - s * A(r,q)
            !         A(r,q) = c * A(r,q) + s * A(r,p)
            !         A(p,r) = A(r,p)
            !         A(q,r) = A(r,q)
            !     end if
            ! end do dl1
            !
            ! Other option: construct P and apply P^T A P
            !
            !
            allocate(Ppq(n,n), stat=ierr)
            if (ierr .ne. 0) stop 'main.f90: Error in allocation of P'
            !
            allocate(A_aux(n,n), stat=ierr)
            if (ierr .ne. 0) stop 'main.f90: Error in allocation of P'
            !
            allocate(PpqT(n,n), stat=ierr)
            if (ierr .ne. 0) stop 'main.f90: Error in allocation of P'
            !
            call identity_mat(Ppq, Ppq)
            !
            Ppq(p,p) = c
            Ppq(p,q) = -s
            Ppq(q,q) = Ppq(p,p)
            Ppq(q,p) = -Ppq(p,q)
            !
            write(*,*) 'Matrix P_pq'
            do i = 1, n
                write(*,'(*(f15.5))') ( Ppq(i,j), j=1, n )
            end do
            write(*,*)
            !
            A_aux = MATMUL(A, Ppq)      ! A_aux = A * P
            PpqT = TRANSPOSE(Ppq)       ! PT = transpose of P
            A = MATMUL(PpqT, A_aux)     ! A' = PT * A * P = PT * A_aux
            !
            write(*,*) 'Updated matrix A'
            do i = 1, n
                write(*,'(*(f15.5))') ( A(i,j), j=1, n )
            end do
            write(*,*)
            !
            ! Deallocate matrices
            !
            if (allocated(Ppq)) deallocate(Ppq, stat=ierr)
            if (ierr /= 0) print *, "Ppq: Deallocation request denied"
            !
            if (allocated(PpqT)) deallocate(PpqT, stat=ierr)
            if (ierr /= 0) print *, "PpqT: Deallocation request denied"
            !
            if (allocated(A_aux)) deallocate(A_aux, stat=ierr)
            if (ierr /= 0) print *, "A_aux: Deallocation request denied"
            !
        end do ml
        !
        return
    end subroutine jacobi     
    !
end module
