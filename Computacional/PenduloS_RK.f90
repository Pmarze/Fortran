!    2021-05-09
!    PenduloS_RK.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o PenduloS_RK.o PenduloS_RK.f90
!    gfortran -o PenduloS_RK.x PenduloS_RK.o
!    ./PenduloS_RK.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./PenduloS_RK.x

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
    REAL, PARAMETER    :: the10=0.9, the20=0.0 !Posición angular inicial, !Velocidad angular inicial
    REAL, PARAMETER    :: h=0.001 !step size
    REAL, PARAMETER    :: tfinal=15 !Tiempo final 
    REAL, DIMENSION(:), ALLOCATABLE :: o, p    
    REAL, DIMENSION(2) :: cons=(/g,l/)
    REAL :: k1the1, k1the2, k2the1, k2the2, k3the1, k3the2, k4the1, k4the2  ! Constantes de RungeKutta 
    REAL :: the2prim, t=0
    INTEGER :: N, i 
    N=CEILING(tfinal/h)                         ! Número de iteraciones
    ALLOCATE(o(N+1))                            ! Vector para almacenar datos
    ALLOCATE(p(N+1))                            ! Vector para almacenar datos
    o(1)=the10                                  ! Condiciones iniciales
    p(1)=the20                                  ! Condiciones iniciales
    !Se abre un archivo de texto
    OPEN(13,file="pendulo_simple.txt")
        DO i=1,N
            !Implementación del método de Runge-Kutta
            k1the1=p(i)
            k1the2=the2prim(o(i),cons)
            k2the1=p(i)+(h/2)*k1the2
            k2the2=the2prim(o(i)+(h/2)*(k1the1),cons)
            k3the1=p(i)+(h/2)*k2the2
            k3the2=the2prim(o(i)+(h/2)*(k2the1),cons)
            k4the1=p(i)+(h)*k3the2
            k4the2=the2prim(o(i)+(h)*(k3the1),cons)
            !Se imprime la iteración 
            WRITE(13,*) t,";",o(i),";",p(i)
            !Se guarda la próxima iteración en los vectores
            o(i+1)=o(i)+(h/6)*(k1the1+2*k2the1+2*k3the1+k4the1)
            p(i+1)=p(i)+(h/6)*(k1the2+2*k2the2+2*k3the2+k4the2)
            t=t+h                   ! aumenta el paso
        END DO
    CLOSE(13)           
END PROGRAM Pendulos

!Función para the2 prima 
REAL FUNCTION the2prim(the1,vec)
REAL,DIMENSION(2) :: vec
REAL ::the1
the2prim = -(vec(1)/vec(2))*sin(the1)
END FUNCTION the2prim