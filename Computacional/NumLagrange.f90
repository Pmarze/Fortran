!    2021-05-09
!    NumLagrange.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que 

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o NumLagrange.o NumLagrange.f90
!    gfortran -o NumLagrange.x NumLagrange.o
!    ./NumLagrange.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./NumLagrange.x

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

PROGRAM NumLagrange
IMPLICIT NONE
    REAL(8) :: x                                ! P(x) valor de x a encontrar
    INTEGER :: col1=3, filas1=6                 ! Número de filas y columnas de los datos
    INTEGER :: col2=3, filas2=6                 ! Número de filas y columnas de los datos
    REAL(8) :: P, Lnk, err                      ! Definiciones de Lagrange P(x) y L_n,k
    REAL(8), DIMENSION(:,:), allocatable :: Datosfit, Datospru
    INTEGER :: i, j, k, n                       ! Indices de sumatoria
    INTEGER :: id,jd                            ! Indices para almacenar los datos a la matriz
    n=filas1-1                                  ! El polinomio de grado k tiene k+1 elementos
    ALLOCATE(Datosfit(filas1,col1))             ! Se crea una matriz de tamaño filas*col
    ALLOCATE(Datospru(filas2,col2))             ! Se crea una matriz de tamaño filas*col
    OPEN(13, file="datospy",status="old")       ! Se lee el archivo a utilizar
        READ(13,*) ((Datosfit(id,jd),jd=1,col1),id=1,filas1) ! Se almacena los datos en la matriz a hacer el fit
    CLOSE(13)
    OPEN(14, file="datosprupy",status="old")    ! Se lee el archivo a utilizar
        READ(14,*) ((Datospru(id,jd),jd=1,col2),id=1,filas2) ! Se almacena los datos en la matriz
    CLOSE(14)    
!    Print*, datosfit(1,1),datosfit(1,2),datosfit(1,3)  ! Corroborar que la lista se almacenó correctamente
!    Print*, datospru(1,1),datospru(1,2),datospru(1,3)  ! Corroborar que la lista se almacenó correctamente
    DO j=1, filas1
        x=Datospru(j,1)                     ! Se calcula para cada x de la lista de prueba
        P=0                                 ! Valor inicial de P(x)
        DO i=1,n                            ! Sumatoria por definición del polinomio
            Lnk=1                           ! Valor inicial de la multiplicatoria
            DO k=1, n                       ! Multiplicatoria para encontrar el valor de L_n,k
                if(k==i)THEN                ! Condición de que i!=j para evitar que L sea indeterminado
                    Lnk=1*Lnk               ! Se multiplica por 1
                ELSE                        ! Se calcula el valor correspondiente de L_n,k
                    Lnk=((x-Datosfit(k,1))/(Datosfit(i,1)-Datosfit(k,1)))*Lnk
                END IF
            END DO
            P=P+Datosfit(i,2)*Lnk           ! Se hace la sumatoria de P
        END DO
        PRINT*, 'P=',P                      ! Se muestra el resultado obtenido
        err=ABS(P-Datospru(j,2))            ! Se muestra el error absoluto para cada dato
        PRINT*, 'error=',err
    END DO    
END PROGRAM NumLagrange