! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 23:02:11 05-10-2022 |
! +----------------------------------------------+

! -----------------------------------------------------------------------------
! PROGRAMA PRINCIPAL
! -----------------------------------------------------------------------------
program fibonacci
    !
    ! Modules
    !
    use fibonacci_series
    !
    ! Deinition of variables
    !
    implicit none
    !
    integer :: i, n
    integer(kind=8) :: fn, fn1, fn2
    real(kind=8) :: phi, appr, error
    !
    ! Formats
    !
    10 format('-------------------------------------------------')
    20 format('=================================================')
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-------------------+'
    write(*,*) '| Program fibonacci |'
    write(*,*) '+-------------------+'
    write(*,*)
    !
    ! Input data
    ! (The last n required)
    !
    write(6,*) 'Calculate the first n-terms of a Fibonacci series'
    write(6,*) 'n?'
    read(5,*) n
    write(6,*)
    !
    ! Initialitations
    ! (F0 and F1 nned to be defined)
    !
    fn2 = 0
    fn1 = 1
    !
    ! Calculation
    !
    ! 1) Evaluate exact phi (golden ratio)
    !
    phi = (1.0_8 + dsqrt(5.0_8))/2.0_8
    !
    ! 2) Compute Fn
    !
    write(6,20)
    write(6,'(2x, "n", 13x, "fn", 5x, "phi", 6x, "approx", 4x, "error")')
    write(6,10)
    do i=2, n
        fn = aux_fibo_n(fn1,fn2)
        !
        ! 3) Compute actual approximation to phi (the simple quotient)
        !
        appr = dble(fn)/dble(fn1)
        !
        ! 4) Check the error on phi
        !
        error = abs(phi - appr)
        !
        ! 5) Output result
        ! (Only the phi part)
        !
        write(6,'(i3.0, i15.0, 3f10.4)') i, fn, phi, appr, error
        !
        ! 6) Update variables
        !
        fn2 = fn1
        fn1 = fn
    end do
    write(6,20)
    write(*,*)
    write(*,*) 'fibo_n(', n, ') =', fibo_n(n)
    !
    stop
endprogram fibonacci
