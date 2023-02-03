! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Wednesday 16:40:46 05-10-2022 |
! +-------------------------------------------+

program matrices_y_modulos
    !
    ! Matrices y modulos
    !
    !
    ! Modulos
    !
    use matmul_mod
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j, n, ierr
    real(kind=8), dimension(:,:), allocatable :: A, B, C
    !
    ! Files
    !
    !
    ! Formats
    !
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+----------------------------+'
    write(*,*) '| Program matrices_y_modulos |'
    write(*,*) '+----------------------------+'
    write(*,*)
    !
    ! Dimensiones
    !
    n = 2
    !
    ! Matriz A
    !
    allocate(A(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of A'
    !
    do i = 1, n
        do j = 1, n
            A(i,j) = i + j
        end do
    end do
    write(*,*) 'Matriz A'
    do i = 1, n
        write(*,'(8f10.2)') A(i,:)
    end do
    write(*,*)
    !
    ! Matriz B
    !
    allocate(B(n,n), stat=ierr)
    if (ierr .ne. 0) stop 'Error in allocation of B'
    !
    do i = 1, n
        do j = 1, n
            B(i,j) = i * j
        end do
    end do
    write(*,*) 'Matriz B'
    do i = 1, n
        write(*,'(8f10.2)') B(i,:)
    end do
    write(*,*)
    !
    ! Matriz C = AB
    !
    write(*,*) 'Matriz C'
    call matrix_mult(A, B, C)
    do i = 1, n
        write(*,*) C(i,:)
    end do
    !
    stop
endprogram matrices_y_modulos
