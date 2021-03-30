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
    INTEGER(8), DIMENSION(2,2) :: b             ! INT(8) para cada componente
    INTEGER(8) :: ffun, j                       ! Variables del problema
    INTEGER :: i                                ! Variable para conteo
    INTEGER :: caso=3 , fibmin=48, fibmax=49
    a(1,1)=1                                    ! Valores de la matriz generadora
    a(1,2)=1
    a(2,1)=1
    a(2,2)=0
    IF (caso==1)THEN                            ! Método de matriz generadora a^n-1
        PRINT*, 'Matriz generadora'
        DO i=fibmin-1,fibmax-1                  ! Se calcularán los números de fibonacci
            CALL fmat(a,i,b)                    ! dentro del rango fmin-fmax
            PRINT*, i+1, b(1,1)                 ! Se imprime el número f_i y su resultado
        END DO
    ELSE IF (caso==2)THEN                       ! Método de función recursiva
        PRINT*, 'Función recursiva'
        DO i=fibmin-1,fibmax-1
            j=INT(i+1,8)                        ! La función necesita un parámetro INT(8)
            print*,i+1,ffun(j)
        END DO
    ELSE IF (caso==3)THEN                       ! Comparar ambos métodos
        DO i=fibmin-1,fibmax-1                  ! Se calcularán los números de fibonacci
            CALL fmat(a,i,b)
            j=INT(i+1,8)
            print*, i+1,'fun=',ffun(j),'mat=',b(1,1)
        END DO
    END IF
END PROGRAM Fibonacci

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

RECURSIVE FUNCTION ffun(n) RESULT (f)           ! Método de función recursiva ffun(n)
    INTEGER(8), INTENT(IN) :: n
    INTEGER(8) :: f
    IF (n<=2)THEN                               ! Si n es menor a dos, el reultado es 1
        f=1
        IF (n<=0)THEN
            f=0
        END IF
    ELSE                                        ! Para cualquier otro caso es necesario 

!        f=ffun(n-1)+ffun(n-2)  !La primera implementación es por definición ffun(n-1) y f(n-2)
!        reescribiendo se puede notar que la ecuación de abajo es mucho más eficiente y obtiene
!        los mismos resultados numéricos, pero a un tiempo muchísimo menor, en caso de querer 
!        comparar, solo debe eliminarse el "!" de la línea 78 y agregarse a la línea 82.                
!        para la primer fórmula con n=45 t=5.59, n=48 t=21.03, n=49 t=35.28
!        para la segund fórmula con n=45 t=0.00, n=48 t=00.01, n=49 t=00.01
!        por lo que es evidente la mejoría.        
        f=2*ffun(n-2)+ffun(n-3)
    END IF
END FUNCTION ffun

SUBROUTINE fmat(a,b,y)                          ! Método de matriz generadora
    INTEGER, DIMENSION(2,2), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: b
    INTEGER(8), DIMENSION(2,2), INTENT(OUT) :: y
    INTEGER :: i, j, n                          ! coordenadas i,j de la matriz y potencia n
    INTEGER(8), DIMENSION(2,2) :: c             ! matriz de paso
    y(1,1)=1                                    ! condición inicial para f_1,f_2
    IF (b==-1)THEN                              ! Condición para f_0
        y(1,1)=0
    END IF
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