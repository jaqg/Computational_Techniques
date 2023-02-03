! +----------------------------------------------+
! | Author: Jose Antonio Quinonero Gris          |
! | Creation date: Wednesday 18:55:50 07-12-2022 |
! +----------------------------------------------+

module mymodule
    !
    ! Module to contain my functions/subrtouines
    !
    implicit none
    !
    ! Subprograms
    !
    contains
        subroutine numcalcpi(totpoints, h, Ncircle, Ntotal, pical, errpi, uf)
            !
            ! Subroutine to compute numerically the value of pi
            !
            ! totpoints     integer, double precision,
            ! h             real, double precision,
            ! Ncircle       integer, double precision,
            ! Ntotal        integer, double precision,
            ! pical         real, double precision,
            ! errpi         real, double precision,
            ! uf            integer.
            !
            ! totpoints: total number of points to iterate
            ! h: dimension of the box
            ! Ncircle: number of points inside the circle of radius h/2
            ! Ntotal: number of -total- points inside the rectangle of side h
            ! pical: calculated value of pi
            ! errpi: absolut error of the calculated value of pi versus the
            !        "exact" value
            ! uf: unit file of the output file
            !
            implicit none
            !
            real(kind=8), intent(in) :: h
            integer(kind=8), intent(in) :: totpoints
            integer, intent(in) :: uf
            integer(kind=8), intent(out) :: Ncircle, Ntotal
            real(kind=8), intent(out) :: pical, errpi
            !
            ! Dummy variables
            !
            integer(kind=8) :: i
            integer :: j
            real(kind=8) :: x, y, d, drecvex, pi
            !
            ! Calculate "exact" value of pi
            !
            pi = 4.0_8*datan(1.0_8)
            !
            ! Initialize
            !
            Ncircle = 0
            Ntotal = 0
            !
            ! Header of results table
            !
            write(uf,'(A,3x,A,3x,A,8x,A)') 'Ntotal','Ncirc','pi calc.','error'
            !
            ! Main loop
            !
            ml1: do i = 1, totpoints, 10
                ml2: do j = 1, 10
                    !
                    ! Generate random number between 0-1 for (x, y)
                    !
                    call random_number(x)
                    call random_number(y)
                    !
                    ! Scalate values of (x, y) to the problem;
                    ! Now, (x, y) go from (0,0) to (1,1) and we need them to go
                    ! from (0, 0) to (h, h) so the points fall inside the h * h
                    ! rectangle.
                    ! We can scalate them doing
                    !
                    ! x -> x * h ; y -> y * h
                    !
                    x = x * h
                    y = y * h
                    !
                    ! The distance of any point (x,y) with respect to the
                    ! center of the circle (or rectangle) (h/2,h/2) is given by
                    !
                    ! d = sqrt( (x - h/2)^2 + (y - h/2)^2 )
                    !
                    d = dsqrt( (x - h/2.0_8)**2 + (y - h/2.0_8)**2 )
                    !
                    ! Distance from the center of the rectangle to the vertix
                    !
                    drecvex = h * dsqrt(2.0_8)/2.0_8
                    !
                    ! Check if the (x,y) point is inside the circle
                    !
                    if (d < h/2.0_8) Ncircle = Ncircle + 1
                    !
                    ! Add the number of total points
                    !
                    Ntotal = Ntotal + 1
                    !
                    ! Calculate value of pi and error vs "exact" value
                    !
                    pical = 4.0_8 * Ncircle/Ntotal
                    errpi = abs(pi - pical)
                    !
                end do ml2
                !
                ! Write results to main output file
                !
                write(uf,'(2(i7,1x),f15.12,f16.12)') Ntotal,Ncircle,pical,errpi
                !
            end do ml1
            !
            return
        end subroutine numcalcpi
end module
