! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 12:22:34 16-10-2022 |
! +-------------------------------------------+

program exercise6
    !
    ! 6. Create a module containing the subroutine written in previous
    ! exercise as well as a function that normalizes to one the array in its
    ! arguments list. Then, write a main program using this module.
    !
    ! Modules
    !
    use mat_mod
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j, n, ierr
    real(kind=8), dimension(:,:), allocatable :: A
    !
    ! Files
    !
    !
    ! Formats
    !
    10 format(' --- INPUT ---')
    20 format(' --- OUTPUT ---')
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-------------------+'
    write(*,*) '| Program exercise6 |'
    write(*,*) '+-------------------+'
    write(*,*)
    !
    ! Input
    !
    write(*,10)
    write(*,*)
    !
    ! Creation of input matrix A
    !
    n = 3 ! dimension of the matrix
    !
    allocate(A(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of A'
    !
    l1: do i = 1, n
        l2: do j = 1, n
            A(i,j) = dble(i+j-2)
        end do l2
    end do l1
    !
    write(*,*) 'Matrix A:'
    call write_mat(A, 6, "F10.2", n)
    write(*,*)
    !
    ! Output
    !
    write(*,20)
    write(*,*)
    !
    ! Normalisation of A
    !
    A = norm_mat(A)
    !
    write(*,*) 'Normalised matrix A:'
    call write_mat(A, 6, "F10.2", n)
    write(*,*)
    !
    write(*,*) 'Numerical check of normalisation:'
    ! do i = 1, n
    !     numcheck = magn_vec( A(:,i) / magn_vec( A(:,i) ) )
    !     if (numcheck == 1.0_8) then
    !         write(*,'(a,i0,a,i0,a,f6.4,a)') &
    !                       ' | A(:,',i,')/|A(:,',i,')| | = ', numcheck, ' -> OK'
    !     else
    !         write(*,'(a,i0,a,i0,a,f6.4,a)') &
    !                       ' | A(:,',i,')/|A(:,',i,')| | = ', numcheck, ' -> X'
    !     end if
    ! end do
    !
    ! write(*,*) 'Or:'
    !
    do i = 1, n
        write(*,'(1x,a,i0,a,f18.16)') 'sum(|A(:,',i,')|**2) = ', &
                                     & sum(abs(A(:,i))**2)
    end do
    !
    write(*,*)
    !
    stop
endprogram exercise6
