! |-------------------------------------------------|
! | Autor: José Antonio Quiñonero Gris              |
! | Fecha de creacion: Thursday 11:35:09 22-09-2022 |
! |_________________________________________________|


! -----------------------------------------------------------------------------
! PROGRAMA PRINCIPAL
! -----------------------------------------------------------------------------
program fibonacci

    !

    ! IMPLICIT REAL*8(A-H,O-Z)
    ! IMPLICIT INTEGER(I-N)

    implicit none

    !
    ! Deinition of variables
    !
    integer :: i, n
    integer*8 :: fn, fn1, fn2
    real(kind=8) :: phi, appr, error     ! equiv to real*8
    real(kind=8), parameter :: one=1.0, two=2.0, five=5.0
    !
    ! Input data
    ! (The last n required
    !
    write(6,*) 'n?'
    read(5,*) n
    !
    ! Initialitations
    ! (F0 and F1 nned to be defined)
    !
    fn2 = 0
    fn1 = 1
    !
    ! Calculation
    !
    ! 1) Evaluate exact phi
    !
    phi = (one + sqrt(five))/two
    !
    ! 2) Compute Fn
    !
    do i=2, n
        fn = fn1 + fn2
        !
        ! 3) Compute actual approximation to phi (the simple quotient)
        !
        appr = dble(fn)/dble(fn1)
        !
        ! 4) Check the error on phi
        !
        error = phi - appr
        !
        ! 5) Output result
        ! (Only the phi part)
        !
        write(6,*) 'n, fn, approx, error : ', i, fn, appr, error, fn/fn1
        !
        ! 6) Update variables
        !
        fn2 = fn1
        fn1 = fn
    end do
    !
    stop
endprogram fibonacci
