!    2020-03-14
!    Fibonacci.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que 


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o Fibonacci.o Fibonacci.f90
!    gfortran -o Fibonacci.x Fibonacci.o
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./Armstrong.x

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
PROGRAM Fibonacci
    IMPLICIT NONE
    INTEGER, DIMENSION(2,2) :: a, b
    INTEGER :: i, fibbonachos=7
    a(1,1)=1
    a(1,2)=1
    a(2,1)=1
    a(2,2)=0
    DO i=0,fibbonachos
        CALL fmat(a,i,b)
        print*, b(1,1)
    END DO
END PROGRAM Fibonacci

SUBROUTINE fmat(a,b,y)
    INTEGER, DIMENSION(2,2), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: b
    INTEGER, DIMENSION(2,2), INTENT(OUT) :: y
    INTEGER :: i, j, n
    INTEGER, DIMENSION(2,2) :: c
    y(1,1)=1
    DO i=1,2
        DO j=1,2
            c(i,j)=a(i,j)
        END DO
    END DO 
    DO n=1, b-1
        DO i=1,2
            DO j=1,2
                y(i,j)=a(i,1)*c(1,j)+a(i,2)*c(2,j)
            END DO
        END DO
        DO i=1,2
            DO j=1,2
                c(i,j)=y(i,j)
            END DO
        END DO 
    END DO
END SUBROUTINE fmat