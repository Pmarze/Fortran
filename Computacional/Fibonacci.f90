!    2021-03-29
!    Fibonacci.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que calcula los números de fibbonacci por el método de 
!    matriz y por el método de función recursiva

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o Fibonacci.o Fibonacci.f90
!    gfortran -o Fibonacci.x Fibonacci.o
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./Fibonacci.x

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
    INTEGER, DIMENSION(2,2) :: a                ! Matriz inicial a y b de resultados
    INTEGER, DIMENSION(2,2) :: b
    INTEGER :: i, ffun                          ! Variables del problema
    INTEGER :: caso=2 , fibmin=3, fibmax=7
    a(1,1)=1                                    ! Valores de la matriz generadora
    a(1,2)=1
    a(2,1)=1
    a(2,2)=0
    IF (caso==1)THEN                            ! para analizar 
        DO i=fibmin-1,fibmax-1                  ! Método de matriz generadora a^n-1
            CALL fmat(a,i,b)
            PRINT*, i, b(1,1)
        END DO
    ELSE IF (caso==2)THEN
        DO i=fibmin-1,fibmax-1                  ! Método de función recursiva
            print*,i+1,ffun(i+1)
        END DO
    END IF
END PROGRAM Fibonacci

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

RECURSIVE FUNCTION ffun(n) RESULT (f)           ! Método de función recursiva ffun(n)
    INTEGER, INTENT(IN) :: n
    INTEGER :: f
    IF (n<=2)THEN                               ! Si n es menor a dos, el reultado es 1
        f=1
    ELSE                                        ! Para cualquier otro caso es necesario 
        f=ffun(n-1)+ffun(n-2)                   ! calcular ffun(n-1) y f(n-2)
    END IF
END FUNCTION ffun

SUBROUTINE fmat(a,b,y)                          ! Método de matriz generadora
    INTEGER, DIMENSION(2,2), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: b
    INTEGER, DIMENSION(2,2), INTENT(OUT) :: y
    INTEGER :: i, j, n                          ! coordenadas i,j de la matriz y potencia n
    INTEGER, DIMENSION(2,2) :: c                ! matriz de paso
    y(1,1)=1                                    ! condición inicial para f_0,f_1
    DO i=1, 2                                   ! Se copia la matriz a en c
        DO j=1, 2
            c(i,j)=a(i,j)
        END DO
    END DO 
    DO n=1, b-1                                 ! se repite b veces la multiplicación de matrices
        DO i=1, 2
            DO j=1, 2
                y(i,j)=a(i,1)*c(1,j)+a(i,2)*c(2,j)  ! fórmula de multiplicación de matrices 2x2
            END DO
        END DO
        DO i=1, 2
            DO j=1, 2
                c(i,j)=y(i,j)                   ! La matriz de paso tiene el valor de y
            END DO
        END DO 
    END DO
END SUBROUTINE fmat

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!