! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Monday 09:01:12 16/01/2023 |
! +-------------------------------------------+

program numerical_linear_algebra

    use num_lin_alg_mod
    implicit none

    allocate(mat (3,3 ), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of A '

    do i = 1, size(mat, dim=1), 1
        do j = 1, size(mat, dim=2), 1
            mat(i,j) = (i+j) * 1.0_8
        end do
    end do
    
    write(unit=*, fmt=*) ''
    write(unit=*, fmt=*) 'Input matrix: mat'
    do i = 1, size(mat, dim=1), 1
        write(unit=*, fmt=*) ( mat(i,j), j=1, size(mat, dim=2) )
    end do

    call SMUL(size(mat, dim=1), size(mat, dim=2),2.0_8,mat,newmat)

    write(unit=*, fmt=*) ''
    write(unit=*, fmt=*) 'SMUL(mat)'
    do i = 1, size(newmat, dim=1), 1
        write(unit=*, fmt=*) ( newmat(i,j), j=1, size(newmat, dim=2) )
    end do

    call MADD(size(mat, dim=1), size(mat, dim=2),mat,mat,newmat)

    write(unit=*, fmt=*) ''
    write(unit=*, fmt=*) 'MADD(mat)'
    do i = 1, size(newmat, dim=1), 1
        write(unit=*, fmt=*) ( newmat(i,j), j=1, size(newmat, dim=2) )
    end do

    call TRANS(size(mat, dim=1), size(mat, dim=2),mat,newmat)

    write(unit=*, fmt=*) ''
    write(unit=*, fmt=*) 'TRANS(mat)'
    do i = 1, size(newmat, dim=1), 1
        write(unit=*, fmt=*) ( newmat(i,j), j=1, size(newmat, dim=2) )
    end do
    
end program numerical_linear_algebra
