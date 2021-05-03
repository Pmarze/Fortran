!    2020-03-14
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que realiza el método de bisección para encontrar el valor de x
!    donde una función f(x)=0, esto con una cantidad n de iteraciones y un valor
!    esperado de precisión, que nos dice que tan cerca de cero está el resultado.


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o biseccion.o biseccion.f90
!    gfortran -o biseccion.x biseccion.o
!    ./biseccion.x

!    Copyright (C) 2021
!    P. D. Martínez Zeceña
!    pabloversion1.0@gmail.com
!
!    This program is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see
!    <http://www.gnu.org/licenses/>.

PROGRAM biseccion
IMPLICIT NONE
    REAL(16) :: a, b, a1, b1, h, prec
    REAL(16) :: fx, fa, error            ! definición de bisección
    REAL(16) :: pi=3.14159               ! Valor aproximado de pi
    INTEGER :: i, n                     ! índices para iteración
    a=0             ! Límite inferior
    b=2*pi          ! Límite superior
    n=1e8           ! Cantidad de iteraciones
    prec=1e-30      ! Precisión esperada
    a1=a            ! limites inferior dentro de la bisección
    b1=b            ! Límites superior dentro de la bisección
    IF(b<a)STOP 'Los límites fueron ingresados al revés'
    DO i=1,n
        h=(b1+a1)/2
        CALL f(h,fx)        ! Se calcula el valor de f(h)
        CALL f(a1,fa)       ! Se calcula el valor de F(a_1)
        IF(fx<=prec .AND. fx>=-1*prec)THEN      ! Si f(h) está muy cercano a cero dentro de la precisión esperada
            error=ABS(b1-a1)/2                  ! Definición del error
            PRINT*, i,'iteraciones'             ! Cantidad de iteraciones necesarias
            PRINT*, '0 de la función en h=',h,'error=',error
            EXIT                                ! Se termina el proceso, no es necesario iterar más veces
        ELSE IF(fx*fa<0)THEN                    ! Si se cumple esta condición, se cambia el límite superior
            b1=h
        ELSE IF(i==n)THEN                       ! Si se realizaron todas las iteraciones sin llegar a la precisión 
            error=ABS(b1-a1)/2                  ! deseada, se imprime el resultado al que se llegó
            PRINT*,'No se obtuvo la precisión esperada'
            PRINT*, 'f(h)=',h,'error=',error
        ELSE                                    ! Para cualquier otro caso, se cambia el límite inferior
            a1=h
        END IF
    END DO
END PROGRAM biseccion

SUBROUTINE f(a,b)                               ! Función para valuar la función f(x)
    REAL(16),INTENT(IN) :: a
    REAL(16),INTENT(OUT) :: b
    !b=a
    b=SIN(a)
END SUBROUTINE f