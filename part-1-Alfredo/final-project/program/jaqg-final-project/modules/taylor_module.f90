! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 17:19:49 13-11-2022 |
! +-------------------------------------------+
module Taylor_module
    implicit none
    contains
        subroutine derivatives(n, y, ders)
            !
            ! Subroutine to compute the derivatives of the LV equations
            !
            ! n => number of derivatives to compute -> y^{(n)'}
            ! y -> y(t) -> y(t,:) => poblations at a time t
            ! ders -> ders(n,:) => evaluated derivatives for each
            !                          individual (x, y, ..., z)
            !         _                           _
            ! ders = | x^{(0)} y^{(0)} ... z^{(0)} |
            !        | x^{(1)} y^{(1)} ... z^{(1)} |
            !        | :       :           :       |
            !        | x^{(n)} y^{(n)} ... z^{(n)} |
            !         -                           -
            !
            use IO, only: params
            !
            implicit none
            !
            integer, intent(in)  :: n
            real(kind=8), dimension(:), intent(in) :: y
            real(kind=8), dimension(:,:), allocatable, intent(out) :: ders
            !
            ! Dummy variables
            !
            integer :: i, j, k, ierr
            real(kind=8) :: xtmp, ytmp
            real(kind=8), dimension(:), allocatable :: y_1, y_2, y_3
            real(kind=8), dimension(:,:), allocatable :: C
            !
            ! You can find more information about how I computed the
            ! derivatives in the 'notes/notes.pdf' document
            !
            ! First, define the coefficients array C
            !
            allocate(C(n,n), stat=ierr)
            if (ierr .ne. 0) stop 'derivatives.f90: Error in allocation of C'
            !
            C = 0.0_8
            C(:,1) = 1.0_8
            !
            Cl1: do i = 1, n-1
                Cl2: do j = 1, n-1
                    C(i+1,j+1) = C(i,j) + C(i,j+1)
                end do Cl2
            end do Cl1
            !
            ! Declare the derivatives array
            !
            allocate(ders(n, size(y)), stat=ierr)
            if (ierr .ne. 0) stop 'derivatives.f90: Error in allocation of &
                                  & ders'
            !
            allocate(y_1(size(y)), stat=ierr)
            if (ierr .ne. 0) stop 'derivatives.f90: Error in allocation of y_1'
            !
            allocate(y_2(size(y)), stat=ierr)
            if (ierr .ne. 0) stop 'derivatives.f90: Error in allocation of y_2'
            !
            allocate(y_3(size(y)), stat=ierr)
            if (ierr .ne. 0) stop 'derivatives.f90: Error in allocation of y_3'
            !
            ! First row of 'ders', ders(1,:) -> x^{(0)} y^{(0)} ... z^{(0)}
            !                                -> y(1)    y(2)    ... y(N)
            !
            ders = 0.0_8
            ders(1,:) = y(:)
            !
            dl1: do i = 1, n-1
                dl2: do j = 1, size(y)-1
                !
                ! I perform the calculation by pairs of prey y(i) and
                ! predators y(i+1)
                !
                ! We can compute y_t^{(n+1)} as
                !
                ! y_t^{(n+1)} = a * y_t^{(n)} +
                !              + b * sum_{k=0}^n C_{nk} y_t^{(n-k)} y_t^{(k)}
                !              + c * sum_{k=0}^n C_{nk} x_t^{(n-k)} y_t^{(k)}
                !
                ! I compute them separately as y_1, y_2 and y_3
                !
                ! y_1
                !
                y_1(j)   = params(1,j) * ders(i,j)
                y_1(j+1) = params(1,j+1) * ders(i,j+1)
                !
                ! y_2
                !
                ytmp = 0.0_8
                xtmp = 0.0_8
                dly2: do k = 1, i
                    ytmp = ytmp + C(i,k) * ders(k,j) * ders(i-k+1,j)
                    xtmp = xtmp + C(i,k) * ders(k,j+1) * ders(i-k+1,j+1)
                end do dly2
                !
                y_2(j)   = params(2,j) * ytmp
                y_2(j+1) = params(2,j+1) * xtmp
                !
                ! y_3
                !
                ytmp = 0.0_8
                xtmp = 0.0_8
                dly3: do k = 1, i
                    ytmp = ytmp + C(i,k) * ders(k,j) * ders(i-k+1,j+1)
                    xtmp = xtmp + C(i,k) * ders(k,j+1) * ders(i-k+1,j)
                end do dly3
                !
                y_3(j)   = params(3,j) * ytmp
                y_3(j+1) = params(3,j+1) * xtmp
                !
                ! Finally, add everything
                !
                ders(i+1,j) = y_1(j) + y_2(j) + y_3(j)
                ders(i+1,j+1) = y_1(j+1) + y_2(j+1) + y_3(j+1)
                !
                end do dl2
            end do dl1
            !
            return
        end subroutine derivatives
        !
        subroutine Taylor(y0, t0, tf, h, nterms, t, y)
            !
            ! Taylor's method:
            ! y(t_{k+1}) = y_{k+1} = y_k + y_k' * h + 1/2 y_k'' * h**2
            !                       + 1/6 y_0''' * h**3 + ...
            !                      = sum_{n=0}^N 1/n! * y_k^{(n)'} * h^n
            ! where
            ! h = t_{k+1} - t_k
            !
            implicit none
            !
            ! Initial values are passed as a 1D array 'y0'
            ! Initial 't0' and final time 'tf', as well as time step 'h', are
            ! passed as scalar
            ! Results are stored in 1D array for time 't', and 2D array for 'y'
            !
            real(kind=8), dimension(:), intent(in) :: y0
            real(kind=8), intent(in) :: t0, tf, h
            integer, intent(in) :: nterms
            real(kind=8), dimension(:), allocatable, intent(out) :: t
            real(kind=8), dimension(:,:), allocatable, intent(out) :: y
            !
            ! Dummy variables
            !
            integer :: i, j, ierr, nt
            real(kind=8), dimension(:,:), allocatable :: ders
            real(kind=8), dimension(:), allocatable :: xtmp
            !
            ! Number of points
            !
            nt = int( (tf - t0)/h ) + 1
            !
            ! Allocate arrays
            !
            allocate(t(nt), stat=ierr)
            if (ierr .ne. 0) stop 'Taylor.f90: Error in allocation of t'
            !
            allocate(y(nt,size(y0)), stat=ierr)
            if (ierr .ne. 0) stop 'Taylor.f90: Error in allocation of y'
            !
            allocate(xtmp(size(y0)), stat=ierr)
            if (ierr .ne. 0) stop 'Taylor.f90: Error in allocation of xtmp'
            !
            y = 0.0_8 ; t = 0.0_8
            !
            ! Initial values
            !
            y(1,:) = y0(:)
            t(1) = t0
            !
            ! Calculation
            !
            l1: do i = 1, nt-1
                !
                ! Time
                !
                t(i+1) = t0 + dble(i) * h
                !
                ! Calculate derivatives
                !
                call derivatives(nterms, y(i,:), ders)
                !
                ! y(t)
                !
                xtmp(:) = y(i+1,:)
                !
                l2: do j = 0, nterms-1
                    !
                    ! y(t_{k+1}) = y_{k+1} = y_k + y_k' * h + 1/2 y_k'' * h**2
                    !                       + 1/6 y_0''' * h**3 + ...
                    !                     = sum_{n=0}^N 1/n! * y_k^{(n)'} * h^n
                    ! where
                    ! h = t_{k+1} - t_k
                    !
                    ! ( Note: n! = gamma(n+1) )
                    !
                    xtmp(:) = xtmp(:) + &
                            & 1.0_8/dgamma(dble(j+1)) * ders(j+1,:) * h**j
                    !
                end do l2
                !
                y(i+1,:) = xtmp(:)
                !
            end do l1
            !
            return
        end subroutine taylor
end module
