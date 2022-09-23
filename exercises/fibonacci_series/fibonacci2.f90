! |-------------------------------------------------|
! | Autor: José Antonio Quiñonero Gris              |
! | Fecha de creacion: Friday 12:02:26 23-09-2022   |
! |_________________________________________________|

! -----------------------------------------------------------------------------
! PROGRAMA PRINCIPAL
! -----------------------------------------------------------------------------
program fibonacci

    ! Programa para la serie de Fibonacci

    ! IMPLICIT REAL*8(A-H,O-Z)
    ! IMPLICIT INTEGER(I-N)

    implicit none

    !
    ! Deinition of variables
    !
    integer :: i, n
    integer*8, dimension(:), allocatable :: fn
    real(kind=8) :: phi, appr, error     ! equiv to real*8
    real(kind=8), parameter :: one=1.0, two=2.0, five=5.0
    !
    ! Input data
    ! (The last n required
    !
    write(6,*) 'n?'
    read(5,*) n
    !
    ! Create the array
    !
    allocate(fn(n))
    !
    ! Initialitations
    ! (F0 and F1 nned to be defined)
    !
    fn(1) = 0
    fn(2) = 1
    ! fn2 = 0
    ! fn1 = 1
    !
    ! Calculation
    !
    ! 1) Evaluate exact phi
    !
    phi = (one + sqrt(five))/two
    !
    ! 2) Compute Fn
    !
    loop1: do i=2, n
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
        !
        ! 7) Condicional
        !
        if (error.eq.0.0) exit loop1
    end do loop1
    !
    stop
endprogram fibonacci
