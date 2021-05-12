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
    INTEGER :: n=2
    REAL :: x=1
    REAL :: P, Lnk
    REAL, DIMENSION(:), allocatable :: Xn, Fn
    INTEGER :: i, j, k
    ALLOCATE(Xn(n))
    ALLOCATE(Fn(n))
    !Xn=(/0.00000,0.00010,0.00020,0.00030,0.00040,0.00050,0.00060,0.00070/)
    !Fn=(/0.00000,0.00010,0.00020,0.00030,0.00040,0.00050,0.00060,0.00070/)
    Xn=(/0.00000,0.00010/)
    Fn=(/0.00000,0.00010/)
    P=0
    DO k=1, n
        Lnk=1
        IF(k==1)THEN
            DO i=1+1, n
                Lnk=Lnk*(x-Xn(i))/(Xn(k)-Xn(i))
            END DO
            print*, Lnk
        ELSE IF(k==n)THEN
            DO i=1, n-1
                Lnk=Lnk*(x-Xn(i))/(Xn(k)-Xn(i))
            END DO
            print*, Lnk            
        ELSE 
            DO i=1, k-1
                Lnk=Lnk*(x-Xn(i))/(Xn(k)-Xn(i))
            END DO
            DO i=k+1, n
                Lnk=Lnk*(x-Xn(i))/(Xn(k)-Xn(i))
            END DO        
        END IF
        P=P+Fn(k)*Lnk
    END DO
    print*, Fn
    print*, P
END PROGRAM NumLagrange

SUBROUTINE f(x,fx)
    REAL, INTENT(IN) :: x
    REAL, INTENT(OUT) :: fx
    fx=SIN(x)
END SUBROUTINE