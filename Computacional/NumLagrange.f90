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
    INTEGER :: col=3, filas=10001
    REAL :: x=2.5
    REAL :: P, Lnk
    REAL, DIMENSION(:,:), allocatable :: Datos, PolL
    INTEGER :: i, k, n
    INTEGER :: id,jd
    REAL :: fx
    !n=4
    n=filas-1                                   ! El polinomio de grado k tiene k+1 elementos
    ALLOCATE(Datos(filas,col))                  ! Se crea una matriz de tamaño filas*col
    ALLOCATE(PolL(filas,1))
    OPEN(13, file="dampOsc_10001",status="old") ! Se lee el archivo a utilizar
        READ(13,*) ((Datos(id,jd),jd=1,col),id=1,filas) ! Se almacena en la matriz
    CLOSE(13)
    P=0
    DO i=1,n
        Lnk=1
        DO k=1, n
            if(k==i)THEN
                Lnk=1*Lnk
            ELSE
                Lnk=((x-Datos(k,1))/(Datos(i,1)-Datos(k,1)))*Lnk
            END IF
        END DO
        P=P+Datos(i,2)*Lnk
    END DO
    Print*, P    
END PROGRAM NumLagrange

SUBROUTINE Func(x,y)
    REAL, INTENT(IN) :: x
    REAL, INTENT(OUT) :: y
    y=SIN(x*3.1415/180)
END SUBROUTINE