!    2020-03-14
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que encuentra los números primos menores a un número n dado.
!    POsteriormente que la conjetura de golfbach se cumple 
!    La conjetura dice: ''Cada entero que puede escribirse como la suma de dos primos, 
!    tambien puede escribirse como la suma de tantos primos como uno desee, 
!    hasta que todos los terminos sean la unidad'' y ''Cada entero mayor que dos puede
!    escribirse como la suma de 3 primos''
!    Además, se observa el comportamiento de v_n,2 y V_n,3 que son las formas distintas
!    como puede escribirse un numero entero con 2 o 3 números primos.

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o goldbach.o goldbach.f90
!    gfortran -o goldbach.x goldbach.o
!    ./goldbach.x

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

PROGRAM goldbach
IMPLICIT NONE
    ! Por el teorema de los números primos tenemos que la cantidad de primos entre 0 y n no excede n/ln(n)
    ! por lo que definimos un arreglo de tamaño n/ln(n) donde se almacenaran todos los primos de ese intervalo.
    INTEGER, DIMENSION(:), allocatable :: lista
    INTEGER :: fint, n=1000000          ! Cantidad de primos a encontrar
    REAL :: freal, nreal
    INTEGER :: i, j, k, l=0
    nreal=REAL(n)
    freal=CEILING(n/(LOG(nreal)-2))   ! Se calcula la cota superior de los primos en ese intervalo
    fint=INT(freal)                     ! Se crea un vector que si mucho tendrá todas sus componentes con un primo correspondiente
    allocate(lista(fint))               ! Se define el tamaño máximo de la lista
    DO i=2, n
        k=0
        DO j=2, i-1
            IF(MOD(i,j)==0)THEN
                k=k+1
            END IF
        END DO
        IF(k==0)THEN
            l=l+1
            lista(l)=i
        END IF
    END DO
    print*, lista
END PROGRAM goldbach