! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Friday 17:27:08 20-01-2023 |
! +-------------------------------------------+

module power_method
    implicit none
    contains

    subroutine is_normalised(v, norm)
        implicit none
        real(kind=8), dimension(:), intent(in) :: v
        logical, intent(out) :: norm
        ! Dummy variables
        integer :: i 
        real(kind=8) :: suma 
        !
        suma = 0.0_8
        dl1: do i = 1, size(v, dim=1)
            suma = suma + abs(v(i))**2
        end do dl1 
        if (suma > 1.0 .or. suma < 1.0) norm=.false.
        !
        return
    end subroutine is_normalised 
    !
    real(kind=8) function magn_vec(v)
        !
        ! Function to calculate the magnitude of a (real) vector
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: v
        integer :: i, n
        real(kind=8) :: suma
        !
        n = size(v,1)
        !
        suma = 0.0_8
        do i = 1, n
            suma = suma + v(i)**2
        end do
        !
        magn_vec = dsqrt(suma)
        !
        return
    end function magn_vec
    !
    function norm_vec(v) result(v_norm)
        !
        ! Function that normalizes to one the vector v (real)
        !
        ! v          real, double precision array
        ! v_norm     real, double precision array
        !
        ! v: vector to be normalised
        ! v_norm: normalised vector
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: v
        real(kind=8), dimension(:), allocatable :: v_norm
        integer :: n, ierr
        !
        n = size(v,1)
        !
        allocate(v_norm(n), stat=ierr)
        if (ierr .ne. 0) stop 'mat_mod.MOD; func norm_vec: &
            & Error in allocation of v_norm'
            !
            v_norm = v/magn_vec(v)
        !
        return
    end function norm_vec
    !
    subroutine mat_vec_prod(A, x0, x1)
        implicit none
        real(kind=8), dimension(:,:), intent(in) :: A
        real(kind=8), dimension(:), intent(in) :: x0
        real(kind=8), dimension(:), intent(out) :: x1
        ! Dummy variables
        integer :: i, j, n, m 
        real(kind=8) :: xtmp 
        !
        ! Check they can be multiplied
        !
        n = size(x0, dim=1)
        m = size(A, dim=2)
        !
        dl1: do i = 1, n
            xtmp = 0.0_8
            dl2: do j = 1, m
                xtmp = xtmp + A(i,j) * x0(j)
            end do dl2
            x1(i) = xtmp
        end do dl1    
        !
        return
    end subroutine mat_vec_prod 
    !
    subroutine dot_prod(u, v, w)
        implicit none
        real(kind=8), dimension(:), intent(in) :: u, v
        real(kind=8), intent(out) :: w
        ! Dummy variables
        integer :: i 
        !
        w = 0.0_8
        dl1: do i = 1, size(u, dim=1)
            w = w + u(i) * v(i)
        end do dl1
        !
        return
    end subroutine dot_prod 
    !
    subroutine PMES(A, x0, threshold, x1, ev, iter)
        implicit none
        real(kind=8), dimension(:,:), intent(in) :: A
        real(kind=8), dimension(:) :: x0
        real(kind=8), intent(in) :: threshold
        real(kind=8), dimension(:), allocatable, intent(out) :: x1
        real(kind=8), intent(out) :: ev
        integer :: iter 
        ! Dummy variables
        integer :: ierr
        real(kind=8) :: old_ev 
        real(kind=8), dimension(:), allocatable :: x2
        logical :: is_norm
        !
        allocate(x1(size(x0, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'power_method.f90: Error in allocation of x1'
        !
        allocate(x2(size(x0, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'power_method.f90: Error in allocation of x2'
        !
        ! Initialize
        !
        iter = 0
        old_ev = 0.0_8
        !
        ! Check normalisation
        !
        call is_normalised(x0, is_norm)
        !
        ! If x0 is not normalised, normalise it
        !
        if (is_norm .neqv. .true.) then
            !
            x0 = norm_vec(x0)
            !
        end if
        !
        ! Main loop
        !
        dl1: do 
            !
            ! Store number of iterations
            !
            iter = iter + 1
            !
            ! x1 = A * x0
            !
            call mat_vec_prod(A, x0, x1)
            !
            ! Check normalisation of x1
            !
            call is_normalised(x1, is_norm)
            !
            ! If x1 is not normalised, normalise it
            !
            if (is_norm .neqv. .true.) then
                !
                x1 = norm_vec(x1)
                !
            end if
            !
            ! ev = (A * x1) * x1 = x2 * x1
            !
            call mat_vec_prod(A, x1, x2)
            call dot_prod(x2, x1, ev)
            !
            ! write(unit=6, fmt=*) 'iter', iter
            ! write(unit=6, fmt=*) 'x0', x0
            ! write(unit=6, fmt=*) 'x1', x1
            ! write(unit=6, fmt=*) 'x2', x2
            ! write(unit=6, fmt=*) 'ev', ev
            ! write(unit=6, fmt=*) 
            !
            ! Check for convergence
            !
            if (abs(ev - old_ev) < threshold) exit dl1
            !
            ! Update variables
            !
            old_ev = ev
            x0 = x1
            !
        end do dl1
        !
        return
    end subroutine PMES 
end module
