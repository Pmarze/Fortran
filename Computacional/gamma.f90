!    2020-03-14
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que 


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o gamma.o gamma.f90
!    gfortran -o gamma.x gamma.o
!    /usr/bin/time -f "%e %M %P" ./gamma.x

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

PROGRAM gamma
IMPLICIT NONE
    REAL(8) :: a, b, z, h           ! Parámetros de la función gamma
    INTEGER(8) :: n                 ! Cantidad de interaciones de la integral
    REAL(8) :: fa,fb, x, fx         ! Parámetros del método de Simpson
    REAL(8) :: S0, S1=0, S2=0, S    ! Parámetros del método de Simpson
    INTEGER(8) :: i                 ! Índice de la sumatoria
    a=0.0                           ! condicion inicial a=0 b=∞
    b=100000000
    n=1e8                          ! Para 1e9 se obtiene una muy buena precisión, para n<1e7 el resultado no sirve
    z=0                           ! valor a calcular en la función gamma
    h=(b-a)/n                       ! Definición método de Simpson
    CALL f(a,z,fa)                  ! Se valúa f(a)
    CALL f(b,z,fb)                  ! se valúa f(b)
    S0=fa+fb                        ! Definición método de Simpson
    DO i=1, n-1                     ! Inicia el método de integración
        x=a+i*h
        IF(MOD(INT(i,4),2)==0)THEN  ! Si i es par MOD(i,2)=0
            CALL f(x,z,fx)
            S2=S2+fx
        ELSE                        ! Si i es impar
            CALL f(x,z,fx)
            S1=S1+fx
        END IF
    END DO
    S=h*(S0+4*S1+2*S2)/3            ! Resultado de la integral
    IF (z==0)THEN
        S=1
    END IF
    PRINT*, S
END PROGRAM gamma

SUBROUTINE f(a,z,b)                 ! Subrutina para valuar una función
    REAL(8), INTENT(IN) :: a, z
    REAL(8), INTENT(OUT) :: b
    b=exp(-a)*a**(z-1)              ! Se ingresa la función deseada de la integral
END SUBROUTINE f