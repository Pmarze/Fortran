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
!    /usr/bin/time -f "%e %M %P" ./goldbach.x

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
    ! Por el teorema de los números primos tenemos que la cantidad de primos entre 0 y n no excede n/(ln(n)-2)
    ! por lo que definimos un arreglo de tamaño n/(ln(n)-2) donde se almacenaran todos los primos de ese intervalo.
    INTEGER, DIMENSION(:), allocatable :: lista
    INTEGER :: fint, n=100             ! Cantidad de primos a encontrar
    INTEGER :: ranginf=1                ! Límite inferior del rango en el cual se corrobora la conjetura
    INTEGER :: rangsup=100             ! Límite superior del rango en el cual se corrobora la conjetura    
    REAL :: freal, nreal
    INTEGER :: i, j, k, l, m            ! Índices para las sumatorias
    INTEGER :: indlist=0, dossum, tresum  ! Índice de la lista de primos, suma de dos y tres primos respectivamente
    INTEGER :: cant, cant2              ! cantidad de primos, cantidad de primos que pueden sumar
    INTEGER :: fordos, fortre           ! formas de sumar dos primos, formas de sumar 3 primos
    nreal=REAL(n)
    freal=CEILING(n/(LOG(nreal)-2))     ! Se calcula la cota superior de los primos en ese intervalo
    fint=INT(freal)                     ! Se crea un vector que si mucho tendrá todas sus componentes con un primo correspondiente
    allocate(lista(fint))               ! Se define el tamaño máximo de la lista
    OPEN(12,file='Vn2.txt')             ! Abre y/o crea el archivo de texto para guardar los datos de Vn,2
    OPEN(13,file='Vn3.txt')             ! Abre y/o crea el archivo de texto para guardar los datos de Vn,3
    DO i=2, n                           ! Se calculan todos los números primos entre 0 y n
        k=0                             ! Se presupone que el número es primo, en caso de fallar se cambia su valor
        DO j=2, i-1
            IF(MOD(i,j)==0)THEN
                k=1                     ! Variable para saber si es primo o no, k=0 -> primo
                EXIT                    ! si hay al menos un divisor ya sabemos que no es primo y no es necesario seguir calculando
            END IF
        END DO
        IF(k==0)THEN                    ! Si ningún número lo divide, es un número primo
            indlist=indlist+1           ! Se añade a la siguiente posición no utilizada de la lista
            lista(indlist)=i
        END IF
    END DO

    DO i=1, fint                        ! Se calcula la cantidad de primos en el intervalo
        IF(lista(i)==0)THEN             ! Las posiciones no utilizadas de la lista tienen un 0
            EXIT
        END IF
        cant=cant+1                     ! Se suma en uno por cada primo encontrado en la lista
    END DO
    print*, cant                        ! Se imprime la cantidad de primos encontrados
    DO i=ranginf, rangsup               ! Se calcula en un rango el comportamiento de la conjetura
        fordos=0                        ! Se presupone que el número no puede escribirse como suma de otros dos primos
        fortre=0                        ! Se presupone que el número no puede escribirse como suma de otros tres primos
        cant2=0
        DO m=1, cant                    ! Se revisa cada posición de los números primos
            IF(lista(m)>i)THEN          ! Si un primo es mayor que i no lo va a sumar y nos gasta tiempo
                EXIT
            END IF
            cant2=cant2+1               ! Se suma en uno por cada primo válido encontrado en la lista
        END DO
        DO j=1, cant2                    ! Por medio de dos ciclos DO se calculan las combinaciones sin repetición de
            DO k=j, cant2                ! las sumas de dos dígitos que pueden formar a cada número "i"
                dossum=lista(j)+lista(k)
                IF (dossum==i)THEN      ! Si el resultado de la suma es igual a i se suma uno a la cantidad de formas de sumar dos
                    !print*, i,'=',lista(j),'+',lista(k)
                    fordos=fordos+1
                END IF
                DO l=k, cant2            ! Por medio de un tercer ciclo DO, se calculan las combinaciones sin repetición de 3 dígitos
                    tresum=lista(j)+lista(k)+lista(l)
                    IF (tresum==i)THEN      ! Si el resultado de la suma es igual a i se suma uno a la cantidad de formas de sumar tres
                        !print*, i,'=',lista(j),'+',lista(k),'+',lista(l)   
                        fortre=fortre+1
                    END IF
                END DO
            END DO
        END DO
        WRITE(12,*) i, fordos           ! Se imprime en el archivo correspondiente el número y la cantidad de formas como puede escribirse
        WRITE(13,*) i, fortre           
    END DO
END PROGRAM goldbach