!    2021-05-17
!    PenduloS_E.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o PenduloS_E.o PenduloS_E.f90
!    gfortran -o PenduloS_E.x PenduloS_E.o
!    ./PenduloS_E.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./PenduloS_E.x

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

!    Copyright (C) 2021
!    Felipe Choy
!    felipechoy1@gmail.com
!    Se tomó como base el programa de  Felipe Choy, este fue modificado
!    y se reorganizó el flujo del programa donde se consideró necesario
!    para obtner un mejor funcionamiento y un código más ordenado.
!    Estos cambios fueron notificados y autorizados por el autor.

PROGRAM Pendulos
    IMPLICIT NONE
    REAL, PARAMETER    :: l=1 , m=1 , g=9.80991 !longitud de la cuerda, masa
    REAL, PARAMETER    :: the10=0.9, the20=0.0  !Posición angular inicial, !Velocidad angular inicial
    REAL, PARAMETER    :: h=0.001 !step size
    REAL, PARAMETER    :: tfinal=15 !Tiempo final    
    REAL, DIMENSION(:), ALLOCATABLE :: o, p
    REAL, DIMENSION(2) :: cons=(/g,l/)
    REAL :: theta2prim, t=0
    INTEGER :: N, i 
    N=CEILING(tfinal/h)                         ! Número de iteraciones
    ALLOCATE(o(N+1))                            ! Vector para almacenar datos
    ALLOCATE(p(N+1))                            ! Vector para almacenar datos

    !Condiciones iniciales
    o(1)=the10
    p(1)=the20
    OPEN(12,file="simple_euler.txt")    
    DO i=1,N
        WRITE(12,*) t,";",o(i),";",p(i)
        o(i+1)=o(i)+h*p(i)
        p(i+1)=p(i)+h*theta2prim(o(i),cons)
        t=t+h
    END DO
    CLOSE(12)    
END PROGRAM Pendulos

!Función para theta2 prima 
REAL FUNCTION theta2prim(theta1, cos)
REAL,DIMENSION(2), INTENT(IN) :: cos
REAL,INTENT(IN) ::theta1
theta2prim = -(cos(1)/cos(2))*sin(theta1)
END FUNCTION theta2prim