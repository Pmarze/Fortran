!    2021-05-09
!    NumNewt.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que 

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o NumNewt.o NumNewt.f90
!    gfortran -o NumNewt.x NumNewt.o
!    ./NumNewt.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./NumNewt.x

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

PROGRAM NumNewt
    IMPLICIT NONE
    INTEGER :: col=2, filas=6
    REAL(8) :: x=-1
    REAL(8) :: P, x_xj
    REAL(8), DIMENSION(:,:), allocatable :: Datos, Difdiv, Multi
    INTEGER :: i, j, k, l, n
    INTEGER :: id,jd
    ALLOCATE(Datos(filas,col))                  ! Se crea una matriz de tamaño filas*col
    ALLOCATE(Difdiv(filas-1,filas-1))
    ALLOCATE(Multi(filas-1,1))
    OPEN(13, file="newtpy",status="old")       ! Se lee el archivo a utilizar
        READ(13,*) ((Datos(id,jd),jd=1,col),id=1,filas) ! Se almacena en la matriz
    CLOSE(13)     
    DO i=1, filas-1
        Difdiv(i,1)=(Datos(i+1,2)-Datos(i,2))/(Datos(i+1,1)-Datos(i,1))
    END DO
    DO j=2, filas-1
        DO k=1, filas-j
            Difdiv(k,j)=(Difdiv(k+1,j-1)-Difdiv(k,j-1))/(Datos(k+j,1)-Datos(k,1))
        END DO
    END DO
    DO i=1, filas
        Multi(i,1)=x-Datos(i,1)
    END DO
    P=Datos(1,2)
    P=P+Difdiv(1,1)*Multi(1,1)
    DO j=2, filas-1
        x_xj=1
        DO k=1, j
            x_xj=x_xj*Multi(k,1)
        END DO
        P=P+Difdiv(1,j)*x_xj
    END DO
    PRINT*, P
    DO n=1, 5
        PRINT*, Difdiv(1,n),Difdiv(2,n),Difdiv(3,n),Difdiv(4,n),Difdiv(5,n)
    END DO 
END PROGRAM NumNewt