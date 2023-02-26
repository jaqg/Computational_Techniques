! +-------------------------------------------+
! | Author: Jose Antonio Quinonero Gris       |
! | Creation date: Sunday 00:37:11 26/02/2023 |
! +-------------------------------------------+

module mymodule
    !
    ! Module to contain my functions/subroutines
    !
    implicit none
    !
    ! Subprograms
    !
    contains
    !
    real(kind=8) function f(x)
        !
        ! Function to integrate
        !
        ! f     real, double precision
        ! x     real, double precision
        !
        ! f: result of the function
        ! x: independent variable
        !
        implicit none
        real(kind=8), intent(in) :: x
        !
        ! $\sin(x^2) - \cos(2x)$
        !
        f = dsin(x**2) - dcos(2.0_8 * x)
        !
        return
    end function f

    real(kind=8) function hk(k, a, b)
        !
        ! Function to compute value of h_k
        !
        ! k     integer, single precision
        !
        ! k: index of h
        !
        implicit none
        !
        integer, intent(in) :: k
        real(kind=8), intent(in) :: a, b 
        !
        hk = (b - a)/(2.0_8**(k - 1))
        !
    end function hk 
    !
    subroutine RombergIA(lil, uil, threshold, Rmat, findex, sindex, convR, &
                        & totiter, uf)
        !
        ! Subroutine for the Romberg integration algorithm (RIA)
        !
        ! lil           real, doule precision
        ! uil           real, doule precision
        ! Rmat          real, doule precision array
        ! findex        integer, single precision
        ! sindex        integer, single precision
        ! convR         real, doule precision
        ! totiter       integer, doule precision
        !
        ! lil: lower integration limit
        ! uil: upper integration limit
        ! Rmat: table of elements R(k,j) of the results of the integral
        ! findex: first index of the converged value of matrix R(k,j) -> k
        ! sindex: second index of the converged value of matrix R(k,j) -> j
        ! convR: converged value -> R(k,j)
        ! totiter: total number of iterations
        !
        implicit none
        !
        real(kind=8), intent(in) :: lil, uil, threshold
        integer, intent(in) :: uf
        real(kind=8), dimension(10,10), intent(out) :: Rmat
        integer, intent(out) :: findex, sindex
        real(kind=8), intent(out) :: convR
        integer(kind=8), intent(out) :: totiter
        !
        ! Dummmy variables
        !
        integer :: i, j, k, iterstep, dummyvar
        real(kind=8) :: Rtmp, diffR
        !
        ! Intro message
        !
        write(uf,'(A)') 'Romberg integration algorithm'
        write(uf,*)
        write(uf,'(A)') 'Full R matrix:'
        write(uf,*)
        !
        ! Initialize variables
        !
        Rmat = 0.0_8
        dummyvar = 0
        !
        ! Compute R_{1,1}
        !
        Rmat(1,1) = hk(1,lil,uil)/2.0_8 * ( f(lil) + f(uil) ) 
        !
        iterstep = 1
        !
        ! Main loop over number of subintervals
        !
        lcr1: do k=2, 10
            !
            ! Iteration step
            !
            iterstep = iterstep + 1
            !
            ! Compute R_{k, 1}; 
            ! first compute the sum_{i=1}^{2^{k-2}} f(a+(2i-1)h_k)
            !
            Rtmp = 0.0_8
            !
            lcr2: do i = 1, 2**(k-2), 1
                Rtmp = Rtmp + f( lil + dble( 2 * i - 1 ) * hk(k,lil,uil) )
            end do lcr2 
            !
            ! Compute final R_{k, 1}
            !
            Rmat(k,1) = 1.0_8/2.0_8 * ( Rmat(k-1,1) + hk(k-1,lil,uil) * Rtmp )
            !
            ! Compute R(k,j)
            !
            ! Initialize variables
            !
            Rtmp = 0.0_8
            !
            lcr3: do j = 2, k
                !
                ! Final value of R
                !
                Rmat(k,j) = Rmat(k,j-1) + &
                          & (Rmat(k,j-1) - Rmat(k-1,j-1))/dble( 4**(j-1) - 1 )
                !
                ! Calculate the difference with the previous value of IRC
                !
                diffR = abs( Rmat(k,j) - Rmat(k,j-1) )
                !
                ! If difference < threshold of convergence, save the converged
                !
                ! Note: from the statement of the exercise, it justs saves the
                ! converged value instead of doing an "infinite loop" until
                ! convergence
                !
                if (diffR<threshold .and. diffR>0.0_8 .and. dummyvar==0) then
                    !
                    ! Stored the converged value and indexes of the matrix
                    !
                    convR = Rmat(k,j)
                    findex = k
                    sindex = j
                    !
                    ! Update a dummy variable so it doesnt keep reassigning 
                    ! findex and sindex after convergence
                    !
                    dummyvar = 1
                    !
                end if
                !
            end do lcr3
            !
        end do lcr1
        !
        ! Total number of iterations
        !
        totiter = iterstep
        !
        return
    end subroutine RombergIA

end module
