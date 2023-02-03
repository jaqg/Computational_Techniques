module cholesky_mod
    implicit none
    character(len=34), parameter :: form10 = "('------------------------------')"
    character(len=34), parameter :: form20 = "('==============================')"
    contains
        !
        ! Cholesky decomposition subroutine
        !
        subroutine cholesky(n,M,L,totiter)
            implicit none
            integer , intent(in) :: n
            real(kind=8), dimension(:,:), intent(inout) :: M
            real(kind=8), dimension(:,:), intent(inout) :: L
            integer, intent(out) :: totiter
            integer :: i, j, k
            !
            loopJ: do J=1, n
                write(*,form20)
                write(*,*) 'J =', J
                write(*,form10)
                !
                ! Print the original matrix
                !
                write(*,*) 'Matrix M'
                lom2: do i = 1, n
                    write(*,'(1000f6.1)') ( M(i,k), k=1, n )
                end do lom2
                write(*,*)
                !
                ! Calculate and print vector L
                !
                write(*,*) 'Vector L'
                loopi: do i=1, n
                    L(i,J) = M(i,J)/sqrt(M(J,J))
                    write(*,'(1000f5.1)') L(i,J)
                end do loopi
                write(*,*)
                !
                ! Update and print matrix M
                !
                write(*,*) 'New matrix M'
                loopi2: do i = 1, n
                    loopk: do k = 1, n
                        M(i,k) = M(i,k) - L(i,j) * L(k,j)
                    end do loopk
                    write(*,'(1000f6.1)') M(i,:)
                end do loopi2
                write(*,form20)
                write(*,*)
                !
                ! Check if new matrix M is null
                !
                check1: do i = 1, n
                    check2: do k = 1, n
                        if (M(i,k).eq.0.0) then
                            cycle
                        else
                            goto 100
                        end if
                    end do check2
                end do check1
                !
                exit loopJ
                100 continue
                totiter = J
                !
            end do loopJ
            return
        end subroutine cholesky
end module
