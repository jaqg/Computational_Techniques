! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Tuesday 11:37:48 04-10-2022 |
! +-------------------------------------------+

program pruebas
    !
    ! Modulos
    !
    use mat_mod
    !
    ! Variable definition
    !
    implicit none
    integer :: i, j
    real(kind=8), dimension(3,3) :: A, B
    real(kind=8), dimension(:,:), allocatable :: C,Cvop
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+------------------+'
    write(*,*) '| Program pruebas  |'
    write(*,*) '+------------------+'
    write(*,*)
    !
    do i = 1, 3
        do j = 1, 3
            A(i,j) = i
            B(i,j) = j
        end do
    end do
    !
    write(*,*) 'Matriz A'
    do i = 1, 3
        write(*,*) A(i,:)
    end do
    write(*,*)
    !
    write(*,*) 'Matriz B'
    do i = 1, 3
        write(*,*) B(i,:)
    end do
    write(*,*)
    !
    write(*,*) 'matrix_mult()'
    call matrix_mult(A, B, C)
    do i = 1, 3
        write(*,*) C(i,:)
    end do
    write(*,*)
    !
    write(*,*) 'mat_outer_prod()'
    C = 0.0_8
    call mat_outer_prod(A, B, C)
    ! loop1: do i = 1, 3
    !     call vec_outer_prod(A(:,i), B(i,:), Cvop)
    !     C = C + Cvop
    ! end do loop1
    do i = 1, 3
        write(*,*) C(i,:)
    end do
    write(*,*)
    !
    stop
endprogram pruebas
