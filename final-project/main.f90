! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 18:19:06 25-10-2022 |
! +--------------------------------------------+

program final_project
    !
    ! Final project
    !
    ! Variable definition
    !
    implicit none
    !
    integer(kind=8) :: i, ierr
    integer(kind=8) :: n
    integer(kind=8), dimension(:), allocatable :: prey, predator
    real(kind=8) :: h, t, t0, tf
    real(kind=8) :: alpha, beta, kappa, lambda
    integer(kind=8) :: LV_prey, LV_predator
    !
    ! Files
    !
    !
    ! Formats
    !
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-----------------------+'
    write(*,*) '| Program final_project |'
    write(*,*) '+-----------------------+'
    write(*,*)
    !
    h = 1.0_8
    !
    tf = 10.0_8
    t0 = 0.0_8
    !
    n = int( (tf - t0)/h )
    write(*,*) 'n =', n
    write(*,*)
    !
    allocate(prey(n+1), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of prey'
    !
    allocate(predator(n+1), stat=ierr)
    if (ierr .ne. 0) stop 'main.f90: Error in allocation of predator'
    !
    prey(1) = 1000
    predator(1) = 200
    !
    alpha  = 5.0D-3
    beta   = 4.0D-4
    kappa  = 3.0D-3
    lambda = 2.0D-4
    !
    write(*,*) 'alpha =', alpha
    write(*,*) 'beta =', beta
    write(*,*) 'kappa =', kappa
    write(*,*) 'lambda =', lambda
    write(*,*)
    !
    lt1: do i = 1, n
        t = t0 + (dble(i-1) * h)
        write(*,'(a,f5.2,1x,a,i0,1x,a,i0)') 't =', t, 'prey = ', prey(i), 'pred = ', predator(i)
        prey(i+1)     = prey(i) + &
                      & int( LV_prey(kappa,lambda,prey(i),predator(i)) * h )
        write(*,*) 'LV_prey =', int( LV_prey(kappa,lambda,prey(i),predator(i)) * h )
        predator(i+1) = predator(i) + &
                      & int( LV_predator(alpha,beta,prey(i),predator(i)) * h )
    end do lt1
    !
    stop
endprogram final_project
