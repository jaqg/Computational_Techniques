      program sumsqinv
!
      implicit none
!
      integer :: i, n
      real(kind=8), parameter :: zero = 0.0_8,
     &                           one = 1.0_8,
     &                           six = 6.0_8,
     &                           pi = 3.14159265358979323846_8,
     &                           thresh = 0.0001_8
      real(kind=8) :: suma = zero, exact, error, suma_i
!
!     Input
!
      write(6,*) 'Sum of the first i terms'
      write(6,*) 'i?'
      read(5,*) i
!
!     Exact solution
!
      exact = pi**2 / six
!
!     The approximated calculation
!
!     (We don't need to write a limit for do, simply because of the if)
!
      n = 0
      do
          n = n + 1
!
          suma  = suma + one / dble(n)**2
          error = abs(suma - exact)
!
          if (i == n) then
              suma_i = suma
          end if
!
          if (error .lt. thresh) exit
      end do
!
!     Output
!
      write(6,'(a,i4,a,f10.6)')
     &        'The sum for the first ', i, ' terms is', suma_i
      write(6,'(a,i7,a,f10.6,a,f10.6)')
     &        'When n = ', n, ' the sum is ', suma,
     &        ' with error equals ', error
!
      stop
      end program
