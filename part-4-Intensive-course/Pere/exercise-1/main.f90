! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 15:14:17 18/01/2023 |
! +----------------------------------------------+

program gaussian_jordan_elimination

    use io
    use mymodule

    implicit none
    !
    ! === START OF THE PROGRAM ===
    !
    write(ofu,'(A)') '+-------------------------------------+'
    write(ofu,'(A)') '| Program gaussian_jordan_elimination |'
    write(ofu,'(A)') '+-------------------------------------+'
    write(ofu,'(A)')
    !
    ! Open input file
    !
    open(unit=10, file='data/input.dat', iostat=ios, &
    status="old", action="read", access="sequential")
    if (ios /= 0) stop "Error opening file data/input.dat"

    ! TODO
    neq = 3
    ofu = 6
    !
    ! Read input matrix
    !
    call read_matrix(M, neq+1, 1, 10, '*')
    !
    ! Allocate and initialize auxiliary matrix, old_M
    !
    allocate(old_M(size(M, dim=1), size(M, dim=2)), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of old_M'
    !
    old_M = M
    !
    ! Write input matrix
    !
    write(unit=ofu, fmt='(A)') 'Input matrix, M ='
    call write_mat(M, ofu, 'f13.8', size(M, dim=2))
    write(unit=ofu, fmt=*)
    !
    ! Main loop
    !
    ml1: do p = 1, size(M, dim=1)
        !
        write(unit=ofu, fmt='(A,i0)') 'iter = ', p
        !
        ! Normalise the leading element, M(p,p)
        !
        if (M(p,p) == 1.0_8 .or. M(p,p) == 0.0_8) goto 100
        write(unit=ofu, fmt='(A)') 'no normalizado'
        do i = 1, size(M, dim=2)
            M(p,i) = M(p,i) / M(p,p)
        end do  
        100 continue
        !
        ! Remove x_1 from all equations except first
        !
        write(unit=ofu, fmt='(A)') 'Normalised matrix M ='
        call write_mat(M, ofu, 'f13.8', size(M, dim=2))
        write(unit=ofu, fmt=*)
        !

        write(*,*) 'old_M(p,p) =', old_M(p,p)
        rl1: do k = 1, size(M, dim=2)
            !
            ! From the p-th row
            !
            M(p,k) = old_M(p,k) / old_M(p,p)
            !
            ! For the rest of the rows
            !
            rl2: do j = 1, size(M, dim=1)
                !
                ! write(*,*) 'old_M(j,k) =', old_M(j,k)
                ! write(*,*) 'old_M(j,p) =', old_M(j,p)
                ! write(*,*) 'old_M(p,k) =', old_M(p,k)
                M(j,k) = old_M(j,k) - old_M(j,p) * ( old_M(p,k)/old_M(p,p) )
                !
            end do rl2
            !
            ! Store values in auxiliary array
            !
            old_M = M
            !
        end do rl1 
        !
        ! Write output matrix
        !
        write(unit=ofu, fmt='(A)') 'Output matrix, M ='
        call write_mat(M, ofu, 'f13.8', size(M, dim=2))
        write(unit=ofu, fmt=*)
        !
    end do ml1 

end program gaussian_jordan_elimination
