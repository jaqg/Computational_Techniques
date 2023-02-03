! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Friday 21:46:24 23-09-2022 |
! +-------------------------------------------+

program cholesky_random
    !
    ! A program to construct a random nxn real symetric positive definite
    ! matrix and decompose it with Cholesky method
    !
    implicit none
    !
    ! Variable definition
    !
    integer :: i, J, k
    integer :: randmin, randmax
    integer :: filA, colB
    integer :: n
    real(kind=8), dimension(:,:), allocatable :: M, L
    real(kind=8), dimension(:), allocatable :: invec, Lvec
    real(kind=8) :: elem, oper
    !
    ! Files
    !
    open(unit=10, file="in-vector.dat")
    !
    ! Formats
    !
    10 format('------------------------------')
    20 format('==============================')
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+------------------+'
    write(*,*) '| Program Cholesky |'
    write(*,*) '+__________________+'
    write(*,*)
    !
    ! Read input dimensions, n
    !
    read(10,*) ! Blank line
    read(10,*) n
    write(*,*) 'The dimension of the matrix is'
    write(*,*) 'n =', n
    write(*,*)
    read(10,*)
    !
    ! Generate random matrix with numbers between randmin and randmax
    !
    randmin = 0
    randmax = 10
    allocate(invec(n))
    call random_number(invec)
    write(*,10)
    write(*,*) 'Random-generated vector'
    do i = 1, n
        !
        ! Make the numbers in u integers (j), between n and m, doing:
        ! j = n + FLOOR((m+1-n)*u)
        !
        invec(i) = randmin + FLOOR((randmax+1-randmin)*invec(i))
        write(*,'(1000f6.1)') invec(i)
    end do
    !
    ! Allocate matrices
    !
    allocate(M(n,n), L(n,n), Lvec(n))
    !
    ! Generate nxn real symetric definite positive matrix doing
    ! M_ij = v_i^T * v_j
    !
    do i = 1, n
        do j = 1, n
            M(j,i) = invec(i) * invec(j)
        end do
    end do
    !
    ! Print the original matrix
    !
    write(*,10)
    write(*,*) 'Original matrix, M0'
    lom1: do i = 1, n
        write(*,'(1000f6.1)') ( M(i,j), j=1, n )
    end do lom1
    write(*,10)
    write(*,*)
    !
    ! Main loop
    !
    write(*,*)
    write(*,*) '--- ITERATIONS ---'
    write(*,*)
    !
    loopJ: do J=1, n
        write(*,20)
        write(*,*) 'J =', J
        write(*,10)
        !
        ! Print the original matrix
        !
        write(*,*) 'Matrix M'
        lom2: do i = 1, n
            write(*,'(1000f6.1)') ( M(i,k), k=1, n )
        end do lom2
        write(*,*)
        !
        ! Calculate and print vector L
        !
        write(*,*) 'Vector L'
        loopi: do i=1, n
            Lvec(i) = M(i,J)/sqrt(M(J,J))
            write(*,'(1000f5.1)') Lvec(i)
            !
            ! Store in matrix L
            !
            L(i,J) = Lvec(i)
        end do loopi
        write(*,*)
        !
        ! Update and print matrix M
        !
        write(*,*) 'New matrix M'
        loopi2: do i = 1, n
            loopk: do k = 1, n
                M(i,k) = M(i,k) - Lvec(i) * Lvec(k)
            end do loopk
            write(*,'(1000f6.1)') ( M(i,k), k=1, n )
        end do loopi2
        write(*,20)
        write(*,*)
        !
        ! Check if new matrix M is null
        !
        check1: do i = 1, n
            check2: do k = 1, n
                if (M(i,k).eq.0.0) then
                    cycle
                else
                    goto 100
                end if
            end do check2
        end do check1
        !
        exit loopJ
        100 continue
        !
    end do loopJ
    !
    write(*,*)
    write(*,*) '--- FINAL RESULTS ---'
    write(*,*)
    !
    write(*,*) 'Total number of iterations:', J
    write(*,*)
    !
    ! Print matrix L
    !
    write(*,10)
    write(*,*) 'Matrix L'
    do i=1, n
        write(*,'(1000f6.1)') ( L(i,j), j=1,n )
    end do
    write(*,10)
    !
    stop
endprogram cholesky_random
