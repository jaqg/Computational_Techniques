! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 00:48:51 18-12-2022 |
! +-------------------------------------------+

module mymodule
    !
    ! Module to contain my functions/subrtouines
    !
    implicit none
    !
    ! Subprograms
    !
    contains
    real(kind=8) function f(vars)
        !
        ! Function to minimize
        !
        ! f        real, double precision
        ! vars     real, double precision, 1D array
        !
        ! f: result of the function
        ! vars: independent variable 1D array
        !
        implicit none
        real(kind=8), dimension(:), intent(in) :: vars
        !
        real(kind=8) :: x, y
        !
        ! $f(x,y) = \sin(x+y) + (x - y)^2 - 1.5x + 3.5y + 3$
        !
        x = vars(1)
        y = vars(2)
        !
        f = dsin(x+y) + (x - y)**2 - 1.5_8 * x + 3.5_8 * y + 3.0_8
        !
        return
    end function f

    real(kind=8) function binomial_coeff(n, m)
        !
        ! Function to calculate the binomial coefficient (n m)
        !
        ! binomial_coeff        real, double precision
        ! n                     innteger, single precision
        ! m                     innteger, single precision
        !
        ! binomial_coeff: result of the function; binomial coeficient (n m)
        ! n: fixed set of elements
        ! m: element to choose from n
        !
        implicit none
        integer, intent(in) :: n, m
        !
        ! Note: since F2008, n! = factorial(n) = Gamma(n+1)
        !
        ! bin(n, m) = n!/(m! (n - m)!)
        !
        binomial_coeff = Gamma( dble(n+1) ) / &
        & ( Gamma( dble(m+1) ) * Gamma( dble(n - m + 1) ) )
        !
        return
    end function binomial_coeff

    subroutine finite_diff(method, n, vars, h, ders)
        !
        ! Subroutine of the finite difference method
        !
        ! method        character
        ! n             integer, single precision
        ! vars          real, double precision, 1D array
        ! h             real, double precision
        ! ders          real, double precision, 1D array
        !
        ! method: method for finite difference (forward, backward, central)
        ! n: n-th derivative f^{(n)'}
        ! vars: points to evaluate f^{(n)'}
        ! h: spacing
        ! ders: result of the subroutine; f^{(n)'}(vars)
        !
        implicit none
        !
        character(len=*), intent(in) :: method
        integer, intent(in) :: n
        real(kind=8), intent(in) :: h
        real(kind=8), dimension(:), intent(in) :: vars
        real(kind=8), dimension(:), allocatable, intent(out) :: ders
        !
        ! Dummy variable
        !
        integer :: i, j, ierr
        real(kind=8) :: res 
        real(kind=8), dimension(:), allocatable :: newvars
        !
        ! Allocate needed arrays
        !
        allocate(newvars(size(vars, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'finite_diff: Error in allocation of newvars'
        !
        allocate(ders(size(vars, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'finite_diff: Error in allocation of ders'
        !
        !
        ! Finite diffference for each method
        !
        if (method=='Forward' .or. method=='forward') then
            !
            ! Forward method
            !
            ! f^{(n)'} (x) = 1/h^n sum_{i=0}^n (-1)^{n-i} bin(n i) f(x+ih, y)
            !
            fdl1: do i = 1, size(vars, dim=1)
                !
                res = 0.0_8
                newvars = vars
                !
                fdl2: do j = 0, n
                    !
                    newvars(i) = vars(i) + dble(j) * h
                    !
                    res = res + &
                    & (-1.0_8)**(n-j) * binomial_coeff(n,j) * f(newvars)
                    !
                end do fdl2
                !
                ders(i) = res/h**n
                !
            end do fdl1
            !
        ! elseif (method=='Backward' .or. method=='backward') then
        !     !
        !     ! Backward method
        !     !
        !     ! f^{(n)'} (x) = 1/h^n sum_{i=0}^n (-1)^i bin(n i) f(x-ih, y)
        !     !
        !     resx = 0.0
        !     bdl1: do i = 0, n
        !         !
        !         resx = resx + &
        !         & (-1.0_8)**i * binomial_coeff(n,i) * f(x - dble(i)*h, y)
        !         !
        !     end do bdl1
        !     !
        !     resx = resx/h**n
        !     !
        !     ! f^{(n)'} (y) = 1/h^n sum_{i=0}^n (-1)^i bin(n i) f(x, y-ih)
        !     !
        !     resy = 0.0_8
        !     bdl2: do i = 0, n
        !         !
        !         resy = resy + &
        !         & (-1.0_8)**i * binomial_coeff(n,i) * f(x, y - dble(i) * h)
        !         !
        !     end do bdl2
        !     !
        !     resy = resy/h**n
        !     !
        elseif (method=='Central' .or. method=='central') then
            !
            ! Central method
            !
            ! f^{(n)'}(x) = 1/h^n sum_{i=0}^n (-1)^i bin(n i) f(x+(n/2-i)h, y)
            !
            ! resx = 0.0_8
            ! cdl1: do i = 0, n
            !     !
            !     resx = resx + &
            !     & (-1.0_8)**i * binomial_coeff(n,i) * &
            !     & f(x + (dble(n)/2.0_8 - dble(i)) * h, y)
            !     !
            ! end do cdl1
            ! !
            ! resx = resx/h**n
            ! !
            cdl1: do i = 1, size(vars, dim=1)
                !
                res = 0.0_8
                newvars = vars
                !
                cdl2: do j = 0, n
                    !
                    newvars(i) = vars(i) + (dble(n)/2.0_8 - dble(j)) * h
                    !
                    res = res + &
                    & (-1.0_8)**(j) * binomial_coeff(n,j) * f(newvars)
                    !
                end do cdl2
                !
                ders(i) = res/h**n
                !
            end do cdl1
        else
            write(unit=*, fmt=*) 'ERROR finite_diff: bad method input'
        end if
        !
        return
    end subroutine finite_diff

    ! subroutine Newton_Raphson(initcoords, threshold, method, h,&
    !     & coord, func, grad, Hessian, conv, mincoord, totiter)
    !     !
    !     ! Subroutine for the Newton-Raphson method
    !     !
    !     ! initcoords    real, double precision, 1D array
    !     ! threshold     real, double precision
    !     ! method        character
    !     ! h             real, double precision
    !     ! coord         real, double precision, 2D array
    !     ! func          real, double precision, 2D array
    !     ! grad          real, double precision, 2D array
    !     ! Hessian       real, double precision, 2D array
    !     ! conv          real, double precision, 2D array
    !     ! mincoord      real, double precision, 1D array
    !     ! totiter       integer, single precision
    !     !
    !     ! initcoords: initial coordinates 1D array
    !     ! threshold: threshold for convergence
    !     ! method: method for the finite difference
    !     ! h: spacing for the finite difference
    !     ! coord: output 2D array with the coordinates for each step
    !     ! func: output 2D array with the function f evaluated at each coord.
    !     ! grad: output 2D array with the gradient of f at each step and coord.
    !     ! Hessian: output 2D array with the Hessian matrix
    !     ! conv: output 2D array with the convergence (error) at each step
    !     ! mincoord: output 1D array with the final coordinates of the minimum
    !     ! totiter: total number of iterations
    !     !
    !     implicit none
    !     !
    !     real(kind=8), dimension(:), intent(in) :: initcoords
    !     character(len=*) :: method
    !     real(kind=8), intent(in) :: h, threshold
    !     real(kind=8), dimension(:,:), allocatable, intent(out) :: coord
    !     real(kind=8), dimension(:,:), allocatable, intent(out) :: func
    !     real(kind=8), dimension(:,:), allocatable, intent(out) :: grad
    !     real(kind=8), dimension(:,:), allocatable, intent(out) :: Hessian
    !     real(kind=8), dimension(:,:), allocatable, intent(out) :: conv
    !     real(kind=8), dimension(:), allocatable, intent(out) :: mincoord
    !     integer, intent(out) :: totiter
    !     ! Dummy variables
    !     !
    !     ! Allocate needed arrays
    !     !
    !     allocate(coord(), stat=ierr)
    !     if (ierr .ne. 0) stop 'main.f90: Error in allocation of coord'
    !     
    !     !
    !     ! 1. Consider an error tolerance -> threshold
    !     !
    !     ! 2. Take a starting point x^k (N-dimensional vector)
    !     !
    !     coord(1,:) = initcoords(:)
    !     !
    !     ! Main loop
    !     !
    !     ml1: do
    !         !
    !     end do ml1
    !     !
    !     return
    ! end subroutine newton_raphson

    ! subroutine Newton_Raphson(initcoords,threshold,method,h,mincoord,totiter)
    !     !
    !     ! Subroutine for the Newton-Raphson method
    !     !
    !     ! initcoords    real, double precision, 1D array
    !     ! threshold     real, double precision
    !     ! method        character
    !     ! h             real, double precision
    !     ! mincoord      real, double precision, 1D array
    !     ! totiter       integer, single precision
    !     !
    !     ! initcoords: initial coordinates 1D array
    !     ! threshold: threshold for convergence
    !     ! method: method for the finite difference
    !     ! h: spacing for the finite difference
    !     ! mincoord: output 1D array with the final coordinates of the minimum
    !     ! totiter: total number of iterations
    !     !
    !     implicit none
    !     !
    !     real(kind=8), dimension(:), intent(in) :: initcoords
    !     character(len=*) :: method
    !     real(kind=8), intent(in) :: h, threshold
    !     real(kind=8), dimension(:), allocatable, intent(out) :: mincoord
    !     integer, intent(out) :: totiter
    !     ! Dummy variables
    !     integer :: i, j, k, ierr 
    !     real(kind=8) :: det 
    !     real(kind=8), allocatable, dimension(:) :: coords, newcoords, grad, &
    !     & dergrad, dergrad2, transgrad, prevgrad
    !     real(kind=8), allocatable, dimension(:,:) :: Hessian, invHessian
    !     !
    !     ! Allocate needed arrays
    !     !
    !     allocate(coords(size(initcoords, dim=1)), stat=ierr)
    !     if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of coords'
    !     !
    !     allocate(newcoords(size(initcoords, dim=1)), stat=ierr)
    !     if (ierr.ne.0) stop 'Newton_Raphson: Error in allocation of newcoords'
    !     !
    !     allocate(grad(size(initcoords, dim=1)), stat=ierr)
    !     if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of grad'
    !     !
    !     allocate(dergrad(size(initcoords, dim=1)), stat=ierr)
    !     if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of dergrad'
    !     !
    !     allocate(dergrad2(size(initcoords, dim=1)), stat=ierr)
    !     if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of dergrad2'
    !     !
    !     allocate(transgrad(size(initcoords, dim=1)), stat=ierr)
    !     if (ierr.ne.0) stop 'Newton_Raphson: Error in allocation of transgrad'
    !     !
    !     allocate(prevgrad(size(initcoords, dim=1)), stat=ierr)
    !     if (ierr.ne.0) stop 'Newton_Raphson: Error in allocation of prevgrad'
    !     !
    !     allocate(Hessian(size(initcoords, dim=1), size(initcoords, dim=1)),&
    !     & stat=ierr)
    !     if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of Hessian'
    !     !
    !     allocate(invHessian(size(initcoords, dim=1), size(initcoords, dim=1)),&
    !     & stat=ierr)
    !     if (ierr.ne.0) stop 'Newton_Raphson: Error in allocation of invHessian'
    !     !
    !     ! 1. Consider an error tolerance -> threshold
    !     !
    !     ! 2. Take a starting point x^k (N-dimensional vector): initcoords
    !     !
    !     coords = initcoords
    !     !
    !     ! Main loop
    !     !
    !     i = 1
    !     prevgrad = 0.0_8
    !     newcoords = 0.0_8
    !     !
    !     ml1: do
    !         !
    !         ! Coordinates to use
    !         !
    !         write(*,'(a,*(f15.8))') ' Initial coordinates =', coords
    !         !
    !         ! 3. Evaluate the gradient
    !         !
    !         3 continue
    !         !
    !         grad = 0.0_8
    !         call finite_diff(method, 1, coords, h, grad)
    !         ! write(*,'(a,*(f15.8))') 'grad =', grad
    !         !
    !         write(*,*)
    !         write(*,*) '========================='
    !         write(*,*) 'i =', i
    !         write(*,*) '========================='
    !         !
    !         ! 4. If gradient > tolerance THEN
    !         !
    !         ! if (all( dabs(grad) > threshold, 1) .eqv. .True.) then
    !         ! if (all( dabs(grad - prevgrad) > threshold, 1) .eqv. .True.) then
    !         if (all( dabs(grad - prevgrad) > threshold, 1) .eqv. .True. &
    !           & all( dabs(grad) > threshold, 1) .eqv. .True.) then
    !             !
    !             ! Compute the Hessian
    !             !
    !             dergrad = 0.0_8
    !             call finite_diff(method, 1, grad, h, dergrad)
    !             !
    !             transgrad = 0.0_8
    !             transgrad = grad( size(grad, dim=1):1:-1 )
    !             !
    !             dergrad2 = 0.0_8
    !             call finite_diff(method, 1, transgrad, h, dergrad2)
    !             ! write(*,'(a,*(f15.8))') 'dergrad2 =', dergrad2
    !             !
    !             Hessian(1,1) = dergrad(1)
    !             Hessian(2,2) = dergrad(2)
    !             Hessian(1,2) = dergrad2(1)
    !             Hessian(2,1) = dergrad2(2)
    !             !
    !             write(*,*)
    !             write(*,*) 'Hessian ='
    !             do j = 1, size(initcoords, dim=1)
    !                 write(*,'(*(f15.8))') ( Hessian(j,k), k=1, size(initcoords, dim=1) )
    !             end do 
    !             !
    !             ! Compute the inverse of the Hessian matrix
    !             !
    !             det = Hessian(1,1)*Hessian(2,2) - (Hessian(1,2) * Hessian(2,1))
    !             write(*,*) 'det =', det
    !             !
    !             invHessian(1,1) =  Hessian(2,2)
    !             invHessian(1,2) = -Hessian(1,2)
    !             invHessian(2,1) = -Hessian(2,1)
    !             invHessian(2,2) =  Hessian(1,1)
    !             !
    !             invHessian = invHessian/det
    !             !
    !             write(*,*)
    !             write(*,*) 'InvHessian ='
    !             do j = 1, size(initcoords, dim=1)
    !                 write(*,'(*(f15.8))') ( invHessian(j,k), k=1, size(initcoords, dim=1) )
    !             end do 
    !             write(*,*)
    !             !
    !             ! Calculate next point
    !             !
    !             write(*,'(a,*(f15.8))') 'matmul =', matmul(invHessian, grad)
    !             write(*,*)
    !             !
    !             newcoords = coords - matmul(invHessian, grad)
    !             write(*,*) 'New coordinates =', newcoords
    !             write(*,*)
    !             !
    !             ! GOTO step 3
    !             !
    !             i = i + 1
    !             !
    !             write(*,*) 'newcoords - coords =', dabs(newcoords - coords)
    !             write(*,*) 'grad - prevgrad =', dabs(grad - prevgrad)
    !             !
    !             prevgrad = grad
    !             coords = newcoords
    !             goto 3
    !             !
    !             ! 5. ELSE GOTO step 6
    !             !
    !         else
    !             goto 6
    !             !
    !         end if
    !         !
    !         ! 6. END program
    !         !
    !         6 exit ml1
    !         !
    !     end do ml1
    !     !
    !     return
    ! end subroutine Newton_Raphson

    subroutine midif(coords, h, Hes)
        implicit none
        real(kind=8), dimension(:), intent(in) :: coords
        real(kind=8), intent(in) :: h
        real(kind=8), dimension(:,:), allocatable, intent(out) :: Hes
        !
        integer :: ierr
        real(kind=8) :: x, y, diff1, diff2, diff3, diff4
        !
        x = coords(1)
        y = coords(2)
        !
        allocate(Hes(size(coords, dim=1), size(coords, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'main.f90: Error in allocation of difs'
        !
        ! diff1 -> d2f/dx2
        ! diff2 -> d2f/dy2
        ! diff3 -> d2f/dxdy
        ! diff4 -> d2f/dydx
        !
        diff1 = ( f( (/ x + 2.0_8 * h, y /) ) + f( (/ x - 2.0_8 * h, y /) ) - 2.0_8 * f( (/ x, y /)) ) / (4.0_8 * h**2)
        !
        diff2 = ( f( (/ x, y + 2.0_8 * h /) ) + f( (/ x, y - 2.0_8 * h /) ) - 2.0_8 * f( (/ x, y /) ) ) / (4.0_8 * h**2)
        !
        diff3 = (f( (/ x+h, y+h /) ) - f( (/ x-h, y+h /) ) - f( (/ x+h, y-h /) ) + f( (/ x-h, y-h /) ) ) / (4.0_8 * h**2)
        !
        diff4 = (f( (/ x+h, y+h /) ) - f( (/ x+h, y-h /) ) - f( (/ x-h, y+h /) ) + f( (/ x-h, y-h /) ) ) / (4.0_8 * h**2)
        !
        Hes(1,1) = diff1
        Hes(2,2) = diff2
        Hes(1,2) = diff3
        Hes(2,1) = diff4
        !
        return
    end subroutine midif 

    subroutine Newton_Raphson(initcoords,threshold,method,h,mincoord,totiter)
        !
        ! Subroutine for the Newton-Raphson method
        !
        ! initcoords    real, double precision, 1D array
        ! threshold     real, double precision
        ! method        character
        ! h             real, double precision
        ! mincoord      real, double precision, 1D array
        ! totiter       integer, single precision
        !
        ! initcoords: initial coordinates 1D array
        ! threshold: threshold for convergence
        ! method: method for the finite difference
        ! h: spacing for the finite difference
        ! mincoord: output 1D array with the final coordinates of the minimum
        ! totiter: total number of iterations
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: initcoords
        character(len=*) :: method
        real(kind=8), intent(in) :: h, threshold
        real(kind=8), dimension(:), allocatable, intent(out) :: mincoord
        integer, intent(out) :: totiter
        ! Dummy variables
        integer :: i, j, k, ierr 
        real(kind=8) :: det
        real(kind=8), allocatable, dimension(:) :: coords, newcoords, grad, &
        & dergrad, dergrad2, transgrad, prevgrad, direc
        real(kind=8), allocatable, dimension(:,:) :: Hessian, invHessian
        !
        ! Allocate needed arrays
        !
        allocate(coords(size(initcoords, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of coords'
        !
        allocate(newcoords(size(initcoords, dim=1)), stat=ierr)
        if (ierr.ne.0) stop 'Newton_Raphson: Error in allocation of newcoords'
        !
        allocate(grad(size(initcoords, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of grad'
        !
        allocate(dergrad(size(initcoords, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of dergrad'
        !
        allocate(dergrad2(size(initcoords, dim=1)), stat=ierr)
        if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of dergrad2'
        !
        allocate(transgrad(size(initcoords, dim=1)), stat=ierr)
        if (ierr.ne.0) stop 'Newton_Raphson: Error in allocation of transgrad'
        !
        allocate(prevgrad(size(initcoords, dim=1)), stat=ierr)
        if (ierr.ne.0) stop 'Newton_Raphson: Error in allocation of prevgrad'
        !
        allocate(Hessian(size(initcoords, dim=1), size(initcoords, dim=1)),&
        & stat=ierr)
        if (ierr .ne. 0) stop 'Newton_Raphson: Error in allocation of Hessian'
        !
        allocate(invHessian(size(initcoords, dim=1), size(initcoords, dim=1)),&
        & stat=ierr)
        if (ierr.ne.0) stop 'Newton_Raphson: Error in allocation of invHessian'
        !
        allocate(direc(size(initcoords, dim=1)), stat=ierr)
        if (ierr.ne.0) stop 'Newton_Raphson: Error in allocation of direc'
        !
        ! 1. Consider an error tolerance -> threshold
        !
        ! 2. Take a starting point x^k (N-dimensional vector): initcoords
        !
        coords = initcoords
        !
        ! Main loop
        !
        i = 0
        prevgrad = 0.0_8
        newcoords = 0.0_8
        ! dummy number so the conv. cond. doest not fulfill
        direc = threshold + 1.0_8 
        !
        write(*,*) 'i  coords  norm2(grad)   norm2(direc)'
        ml1: do
            !
            i = i + 1
            !
            ! write(*,*)
            ! write(*,*) '========================='
            ! write(*,*) 'i =', i
            ! write(*,*) '========================='
            !
            ! Coordinates to use
            !
            ! write(*,'(a,*(f15.8))') ' Input coordinates =', coords
            !
            ! 3. Evaluate the gradient
            !
            grad = 0.0_8
            call finite_diff(method, 1, coords, h, grad)
            !
            ! 4. If norm(gradient)<tolerance and norm(direc)<tolerance; exit
            !
            if (norm2(grad)<threshold .or. norm2(direc)<threshold) exit ml1
            call midif(coords, h, Hessian)
            !
            ! write(*,*)
            ! write(*,*) 'Hessian ='
            ! do j = 1, size(initcoords, dim=1)
            !     ! write(*,'(*(f15.8))') ( Hessian(j,k), k=1, size(initcoords, dim=1) )
            !     write(*,*) ( Hessian(j,k), k=1, size(initcoords, dim=1) )
            ! end do 
            !
            ! Compute the inverse of the Hessian matrix
            !
            det = Hessian(1,1)*Hessian(2,2) - (Hessian(1,2) * Hessian(2,1))
            ! write(*,*) 'det =', det
            !
            invHessian(1,1) =  Hessian(2,2)
            invHessian(1,2) = -Hessian(1,2)
            invHessian(2,1) = -Hessian(2,1)
            invHessian(2,2) =  Hessian(1,1)
            !
            invHessian = invHessian/det
            !
            ! write(*,*)
            ! write(*,*) 'InvHessian ='
            ! do j = 1, size(initcoords, dim=1)
            !     write(*,'(*(f15.8))') ( invHessian(j,k), k=1, size(initcoords, dim=1) )
            ! end do 
            ! write(*,*)
            !
            ! Calculate next point
            !
            direc = - matmul(invHessian, grad)
            !
            ! write(*,'(a,*(f15.8))') 'direc =', direc
            ! write(*,*)
            newcoords = coords + direc
            ! write(*,*) 'New coordinates =', newcoords
            ! write(*,*)
            !
            ! write(*,'(i0, *(f15.8))') i, coords, norm2(grad), norm2(newcoords - coords)
            write(*,'(i0, *(4x, e15.8))') i, coords, grad, Hessian(1,1), Hessian(1,2), Hessian(2,1), Hessian(2,2), norm2(grad)
            !
            ! Update variables
            !
            prevgrad = grad
            coords = newcoords
            !
            ! write(*,*) 'norm2(grad) =', norm2(grad), 'norm2(direc) =', norm2(direc)
            !
            ! 
            !
            !
        end do ml1
        !
        return
    end subroutine Newton_Raphson
end module
