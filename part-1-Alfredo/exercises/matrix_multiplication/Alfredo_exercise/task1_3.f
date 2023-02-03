      program matrix_mul
!
      implicit none
!
      integer :: i, j, k
      integer :: m, n, p, nrowA, ncolA, nrowB, ncolB, ierr
      real(kind=8), allocatable, dimension(:,:) :: A, B, C
      real(kind=8) :: xtmp
      real(kind=8), parameter :: zero = 0.0
!
!     Input
!
      write(6,*) 'Dimensions of A?'
      read(5,*) nrowA, ncolA
      write(6,*) 'Dimensions of 6?'
      read(5,*) nrowB, ncolB
!
!     Check dimensions
!
      if (ncolA .ne. nrowB) then
          write(6,*) 'Error: ncolA must be equal to nrowB'
      else
          m = nrowA ; n = ncolA ; p = ncolB
          allocate(A(m,n), B(n,p), C(m,p), stat=ierr)
          if (ierr .ne. 0) stop 'Error in allocation'
      end if
!
!     Read A and B
!
      write(6,*) 'A matrix?'
      do i = 1, m
          read(5,*) ( A(i,j), j = 1, n )
      end do
!
      write(6,*) 'B matrix?'
      do i = 1, n
          read(5,*) ( B(i,j), j = 1, p )
      end do
!
!     Calculation
!
      do j = 1, p
          do i = 1, m
              xtmp = zero
              do k = 1, n
                  xtmp = xtmp + A(i,k) * B(k,j)
              end do
              C(i,j) = xtmp
          end do
      end do
!
! Output
!
      write(6,*) 'C matrix'
      do i = 1, m
          write(6,100) ( C(i,j), j = 1, p )
 100      format(8f10.5)
      end do
!
      stop
      end program
