!
! Subrutina creada por Alejando Luís García Muñoz
! Fecha: 01/03/2023
! puto amo
! coding, coding...
!
program columnas
implicit none
integer:: nc,err
real::kk !kk tiene el formato de tus celdas
character(len=30)::fmt
open(69,file='mat.inp')
nc=0
err=0
fmt='(X,F0)' !Cambiar 'I2' por el formato de tus celdas
do
if(nc/=0) read(69,fmt,iostat=err,advance='no') kk
if(err/=0) exit
nc=nc+1
end do
write(6,*) 'Tu matriz tiene',nc,'columnas'
end program columnas
