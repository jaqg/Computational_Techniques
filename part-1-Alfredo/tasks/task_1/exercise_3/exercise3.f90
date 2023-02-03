! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Saturday 20:34:06 24-09-2022 |
! +---------------------------------------------+

program exercise3
    !
    ! Write a Fortran program to calculate the product of the two matrices
    ! given as input. The input should also include the dimensions of the
    ! input matrices, so that the first step in the program will be to check
    ! whether such matrices can actually be multiplied.
    !
    ! The elements of the product nxn matrix C = AB are given by
    ! C_i,j = sum_k=1^n A_i,k B_k,j
    !
    ! Variable definition
    !
    implicit none
    !
    integer :: i, j, k
    integer :: n1, m1, n2, m2, n
    real(kind=8), dimension(:,:), allocatable :: A, B, C
    real(kind=8) :: elem, oper
    !
    ! Files
    !
    open(unit=10, file="in-exercise3.dat")
    open(unit=11, file="out-exercise3.dat")
    !
    ! === START OF THE PROGRAM ===
    !
    write(11,*) '+-------------------+'
    write(11,*) '| Program exercise3 |'
    write(11,*) '+-------------------+'
    write(11,*)
    !
    ! Read input
    !
    read(10,*)
    read(10,*) n1, m1
    read(10,*)
    allocate(A(n1,m1))
    read(10,*) A
    read(10,*)
    read(10,*) n2, m2
    read(10,*)
    allocate(B(n2,m2))
    read(10,*) B
    !
    ! Close input files
    !
    close(10)
    !
    ! Print input matrices
    !
    write(11,*) 'The input matrices are:'
    write(11,*)
    write(11,*) 'A ='
    do i = 1, n1
        write(11,'(10000f6.1)') A(i,:)
    end do
    write(11,*)
    write(11,*) 'B ='
    do i = 1, n2
        write(11,'(10000f6.1)') B(i,:)
    end do
    write(11,*)
    !
    ! Check if matrices can actually be multiplied
    !
    if (n1 /= m2) then
        write(11,*) "The matrices can't be multiplied."
        write(11,*) 'Check the dimension of the matrices.'
    else
        n = n1
    end if
    !
    ! Product of matrices
    !
    allocate(C(n,n))
    elem = 0
    loop1: do i = 1, n
        loop2: do j = 1, n
            loop3: do k = 1, n
                oper = A(i,k) * B(k,j)
                elem = elem + oper
                C(i,j) = elem
            end do loop3
        end do loop2
    end do loop1
    !
    ! Print result matrix C
    !
    write(11,*) 'The result matrix C = AB is:'
    write(11,*)
    write(11,*) 'C ='
    do i = 1, n
        write(11,'(1000f10.2)') C(i,:)
    end do
    write(11,*)
    !
    ! Close output file
    !
    close(11)
    !
    write(*,*) 'Results writen to file "out-exercise3.dat"'
    !
    stop
endprogram exercise3
