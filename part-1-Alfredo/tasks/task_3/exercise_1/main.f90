! +--------------------------------------------+
! | Author: Jose Antonio Quinonero Gris        |
! | Creation date: Tuesday 11:40:14 11-10-2022 |
! +--------------------------------------------+

program exercise1
    !
    ! 1. Write a program that converts into an integer by using INT and NINT
    ! the result of the division of the following real numbers:
    ! 5.9/2.0; 4.9/2.0; 3.0/2.0 and 2.9/2.0.
    !
    ! Variable definition
    !
    implicit none
    !
    ! Formats
    !
    10 format(1x,a,1x,i0)
    !
    ! === START OF THE PROGRAM ===
    !
    write(*,*) '+-------------------+'
    write(*,*) '| Program exercise1 |'
    write(*,*) '+-------------------+'
    write(*,*)
    !
    write(*,*) 'Operations with real results:'
    write(*,*) '5.9/2.0 =', 5.9/2.0
    write(*,*) '4.9/2.0 =', 4.9/2.0
    write(*,*) '3.0/2.0 =', 3.0/2.0
    write(*,*) '2.9/2.0 =', 2.9/2.0
    write(*,*)
    !
    write(*,*) 'Operations with INT results:'
    write(*,10) '5.9/2.0 =', int(5.9/2.0)
    write(*,10) '4.9/2.0 =', int(4.9/2.0)
    write(*,10) '3.0/2.0 =', int(3.0/2.0)
    write(*,10) '2.9/2.0 =', int(2.9/2.0)
    write(*,*)
    !
    write(*,*) 'Operations with NINT results:'
    write(*,10) '5.9/2.0 =', nint(5.9/2.0)
    write(*,10) '4.9/2.0 =', nint(4.9/2.0)
    write(*,10) '3.0/2.0 =', nint(3.0/2.0)
    write(*,10) '2.9/2.0 =', nint(2.9/2.0)
    write(*,*)
    !
    write(*,*) 'We can see how INT does the truncation to whole number, while &
              & NINT nearest the whole number.'
    write(*,*)
    !
    stop
endprogram exercise1
