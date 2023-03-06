! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 13:11:23 01/03/2023 |
! +----------------------------------------------+

module polynomial_regression
    !
    ! Module containing needed subprograms to perform a polynomial regression
    !
    implicit none
    !
    contains

    subroutine create_augmented_mat(data, poldeg, augmat)
        !
        ! Subroutine to set up the augmented matrix by calculating all
        ! necessary sums for a polynomial of arbitrary degree 'poldreg'
        ! that fits into 'data'
        !
        implicit none
        !
        real(kind=8), dimension(:,:), intent(in) :: data
        integer, intent(in) :: poldeg
        real(kind=8), dimension(:,:), allocatable, intent(out) :: augmat
        ! Dummy variables
        integer :: i, j, ierr 
        !
        ! Allocate needed arrays
        !
        allocate(augmat(poldeg+1, poldeg+2), stat=ierr)
        if (ierr .ne. 0) stop &
            & 'create_augmented_mat: Error in allocation of augmat'
        !
        aml1: do i = 1, poldeg+1
            aml2: do j = i, poldeg+1
                !
                augmat(j,i) = sum(data(:,1)**(i+j-2))
                augmat(i,j) = augmat(j,i)
                !
            end do aml2
            !
            augmat(i,poldeg+2) = sum( data(:,1)**(i-1) * data(:,2) )
            !
        end do aml1
        !
        return
    end subroutine create_augmented_mat 

    real(kind=8) function fitted_poly(coefs, x)
        !
        ! Function for the fitted polynomial
        !
        implicit none
        real(kind=8), dimension(:), intent(in) :: coefs
        real(kind=8), intent(in) :: x
        ! Dummy variables
        integer :: i 
        !
        fitted_poly = 0.0_8
        !
        fpl1: do i = 1, size(coefs, dim=1)
            fitted_poly = fitted_poly + coefs(i) * x**(i-1)
        end do fpl1
        !
        return
    end function fitted_poly 

    subroutine R_coeff(xdat, ydat, coefs, R2)
        !
        ! Subroutine to calculate R^2
        !
        implicit none
        !
        real(kind=8), dimension(:), intent(in) :: xdat, ydat, coefs
        real(kind=8), intent(out) :: R2
        ! Dummy variables
        integer :: i, n 
        real(kind=8) :: SSres, SStot, mean_y
        !
        ! Check that xdata and ydata are the same size
        !
        n = size(xdat, dim=1)
        !
        if (n .ne. size(ydat, dim=1)) stop &
            & 'ERROR R_coeff: x and y are not the same size'
        !
        ! Calculate the residual sum pof squares
        !
        SSres = 0.0_8
        rl1: do i = 1, n
            SSres = SSres + ( ydat(i) - fitted_poly(coefs, xdat(i)) )**2
        end do rl1
        !
        ! Calculate the total sum of squares
        !
        mean_y = sum(ydat)/dble(n)
        !
        SStot = 0.0_8
        tl1: do i = 1, n
            SStot = SStot + ( ydat(i) - mean_y )**2
        end do tl1
        !
        ! Calculate R^2
        !
        R2 = 1.0_8 - ( SSres/SStot )
        !
        return
    end subroutine R_coeff 

end module
