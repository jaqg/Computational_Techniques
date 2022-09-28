! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 18:20:47 24-09-2022 |
! +---------------------------------------------+

program exercise2
    !
    ! Write a Fortran program to compute the dot and the cross products of
    ! two vectors (given as input) of R^3.
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i
    real(kind=8), dimension(3) :: v1, v2
    real(kind=8) :: dotprod, oper
    real(kind=8) :: crossprod, iv, jv, kv
    !
    ! Files
    !
    open(unit=10, file="in-exercise2.dat")
    open(unit=11, file="out-exercise2.dat")
    !
    ! === START OF THE PROGRAM ===
    !
    write(11,*) '+-------------------+'
    write(11,*) '| Program exercise2 |'
    write(11,*) '+-------------------+'
    write(11,*)
    !
    ! Read input vectors
    !
    read(10,*)
    read(10,*) v1
    read(10,*) v2
    close(10)
    !
    ! Write input vectors
    !
    write(11,*) 'The input vectors are:'
    write(11,'(9x, "i", 7x, "j", 7x, "k")')
    write(11,'(1x, "v1 =", 3f8.2)') v1
    write(11,'(1x, "v1 =", 3f8.2)') v2
    write(11,*)
    !
    ! Dot product
    !
    dotprod = 0.0_8
    loop1: do i = 1, 3
        oper = v1(i) * v2(i)
        dotprod = dotprod + oper
    end do loop1
    write(11,*) 'The dot product is:'
    write(11,'(1x, a, f8.2)') "v1·v2 =", dotprod
    write(11,*)
    !
    ! Cross product
    !
    iv = v1(2)*v2(3) - v2(2)*v1(3)
    jv = -(v1(1)*v2(3) - v2(1)*v1(3))
    kv = v1(1)*v2(2) - v2(1)*v1(2)
    !
    write(11,*) 'The cross product is:'
    write(11,'(1x,"v1xv2 =",f8.2,"i",SP,f8.2,"j",SP,f8.2,"k")') iv, jv, kv
    write(11,*)
    !
    ! Close output file
    !
    close(11)
    !
    write(*,*) 'Results writen in file "out-exercise2.dat"'
    !
    stop
endprogram exercise2
