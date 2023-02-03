! +---------------------------------------------+
! | Author: Jose Antonio Quinonero Gris         |
! | Creation date: Thursday 16:07:50 10-11-2022 |
! +---------------------------------------------+
subroutine Taylor(xn, yn, xnp1, ynp1)
    !
    ! Taylor's method:
    ! y(t) = y_0 + y_0' * h + 1/2 y_0'' * h**2 + 1/6 y_0''' * h**3
    !
    implicit none
    !
    real(kind=8), intent(in) :: xn, yn
    real(kind=8), intent(out) :: xnp1, ynp1
    !
    ! Hacer que calcule las derivadass en una subrutina externa, que las guarde
    ! por ejemplo en un array y que pase como el argumento dicho array a esta
    ! subrutina, de manera que pueda escribir la expansion de Taylor como un
    ! bucle de i=1 hasta Nterm, donde Nterm = size(array_derivadas) es el
    ! numero de terminos a incluir en la expasion. De esta manera, esta
    ! subrutina valdria para cualquier funcion cuyas derivadas se calculen
    ! en una funcion/subrutina externa
    !
    !
    ! Taylor expansion
    !
    xnp1 = xn + xp1 * h + 1.0_8/2.0_8 * xp2 * h**2 + 1.0_8/6.0_8 * xp3 * h**3
    ynp1 = yn + yp1 * h + 1.0_8/2.0_8 * yp2 * h**2 + 1.0_8/6.0_8 * yp3 * h**3
    !
    return
end subroutine Taylor
