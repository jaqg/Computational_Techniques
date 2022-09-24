! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 12:50:32 24-09-2022 |
! +---------------------------------------------+

program exercise1
    !
    ! Write a program to calculate the sum of the squared inverse (1/n2) for
    ! the n first natural numbers. Check by increasing n that the sum
    ! converges to pi**2/6. How many terms must be included in order the error
    ! to be less than 0.0001?
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: n
    real(kind=8) :: pi, exac, suma, res, err
    !
    ! Files
    !
    open(unit=10, file="out-exercise1.dat")
    !
    ! Formats
    !
    10 format(' ----------------------------------------')
    !
    ! === START OF THE PROGRAM ===
    !
    write(10,*) '+-------------------+'
    write(10,*) '| Program exercise1 |'
    write(10,*) '+-------------------+'
    write(10,*)
    !
    ! Define constants
    !
    pi = 4.0_8 * datan(1.0_8)
    exac = pi**2/6.0_8
    !
    ! Write first row
    !
    write(10,10)
    write(10,'(9x, "n", 8x, "1/n**2", 10x, "error")')
    write(10,10)
    !
    ! Main loop
    !
    suma = 0
    loop1: do n = 1, 1000000
        res = 1.0_8/dble(n)**2
        suma = suma + res
        err = exac - suma
        write(10,'(i10.1, 2f15.6)') n, suma, err
        if (err<0.0001) exit
    end do loop1
    !
    ! Write results
    !
    write(10,10)
    write(10,*)
    write(10,'(1x, "pi**2/6 =", f9.6)') exac
    write(10,*) 'The number of terms included in order the'
    write(10,*) 'error to be less than 0.0001 is:'
    write(10,'(1x, "n =", i10.1)') n
    write(10,*)
    !
    write(*,*) 'Results stored in file "out-exercise1.dat"'
    !
    stop
endprogram exercise1
