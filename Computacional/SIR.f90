!    2021-05-09
!    SIR.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o SIR.o SIR.f90
!    gfortran -o SIR.x SIR.o
!    ./SIR.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./SIR.x

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

PROGRAM SIR
IMPLICIT NONE 
    REAL,PARAMETER :: beta=0.5, gamma=0.1111111, mu=0.001167    ! Constantes del problema
    REAL,PARAMETER :: A=1449401, N= 1449401, h=0.005, tfinal=40
    REAL, DIMENSION(:), allocatable :: o,p,q        ! Vectores para almacenar resultados de I,S,R    
    REAL, DIMENSION(7) :: const                     ! Vector para almacenar constantes
    REAL :: S0, I0, R0                              ! Condiciones iniciales
    REAL :: S, I, R                                 ! Funciones a utilizar
    INTEGER :: j, eNe                               ! Indice de sumatoria
    S0 =1446093/N
    I0 = 1885/N
    R0 = 1423/N
    eNe=INT(CEILING(tfinal/h)+1)                    ! Cota de los vectores de datos 
    const=(/beta,gamma,mu,A,N,h,tfinal/)            ! Constantes a utilizar
    ALLOCATE(o(eNe))                                ! Tamaño del vector
    ALLOCATE(p(eNe))
    ALLOCATE(q(eNe))
    o(1)=S0                                         ! Condicion inicial S0
    p(1)=I0                                         ! Condicion inicial I0
    q(1)=R0                                         ! Condicion inicial R0
    !OPEN(12,file='Resultados_SIR.txt')             ! Datos tabulados en un solo archivo
    OPEN(13,file="S.txt")                           ! Datos individuales
    OPEN(14,file="I.txt")
    OPEN(15,file="R.txt")    
    DO j=2, eNe                                     ! Calculo de cada instancia de S,I,R
        o(j)=S(o(j-1),p(j-1),q(j-1),const)
        p(j)=I(o(j-1),p(j-1),q(j-1),const)
        q(j)=R(o(j-1),p(j-1),q(j-1),const)
    END DO
    DO j=1, eNe                                     ! Impresión de resultados
        !WRITE(12,*) j*h,' ',o(j),' ',p(j),' ',q(j) ! Impresión de datos en un solo archivo
        WRITE(13,*) j*h," ",o(j)
        WRITE(14,*) j*h," ",p(j)
        WRITE(15,*) j*h," ",q(j)        
    END DO
END PROGRAM SIR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Funciones S, I, R que dependen de las constante k_i, m_i, l_i

REAL FUNCTION S(x,y,z,cons)
    REAL :: x,y,z
    REAL, DIMENSION(7) :: cons
    REAL,EXTERNAL :: k1,k2,k3,k4
    S=x+(cons(6)/6)*(k1(x,y,z,cons)+2*k2(x,y,z,cons)+2*k3(x,y,z,cons)+k4(x,y,z,cons))
END FUNCTION

REAL FUNCTION I(x,y,z,cons)
    REAL::x,y,z
    REAL, DIMENSION(7) :: cons    
    REAL,EXTERNAL:: l1,l2,l3,l4
    I = y + (cons(6)/6)*(l1(x,y,z,cons)+2*l2(x,y,z,cons)+2*l3(x,y,z,cons)+l4(x,y,z,cons))
END FUNCTION

REAL FUNCTION R(x,y,z,cons)
    REAL::x,y,z
    REAL, DIMENSION(7) :: cons    
    REAL,EXTERNAL:: m1,m2,m3,m4
    R = z + (cons(6)/6)*(m1(x,y,z,cons)+2*m2(x,y,z,cons)+2*m3(x,y,z,cons)+m4(x,y,z,cons))
END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Funciones k_i, m_i, l_i

REAL FUNCTION k1(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    z=z*1
    k1= cons(4)-cons(3)*x-(cons(1)*x*y/cons(5))
END FUNCTION k1

REAL FUNCTION l1(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    z=z*1
    l1= (cons(1)*x*y/cons(5))-(cons(3)+cons(2))*y
END FUNCTION l1

REAL FUNCTION m1(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    x=x*1
    m1 = cons(2)*y-cons(3)*z
END FUNCTION m1

REAL FUNCTION k2(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    REAL,EXTERNAL::k1,l1
    REAL :: k21, k22
    k21=cons(4)-cons(3)*(x+(cons(6)*k1(x,y,z,cons)/2))
    k22=((cons(1)/cons(5))*(y+(cons(6)*l1(x,y,z,cons)/2))*(x+(cons(6)*k1(x,y,z,cons)/2)))
    k2=k21-k22
END FUNCTION k2

REAL FUNCTION l2(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    REAL,EXTERNAL::k1,l1
    REAL :: l21, l22
    l21=(cons(1)/cons(5))*(y+(cons(6)*l1(x,y,z,cons)/2))*(x+(cons(6)*k1(x,y,z,cons)/2))
    l22=((cons(3)+cons(2))*(y+(cons(6)*l1(x,y,z,cons)/2)))
    l2=l21-l22
END FUNCTION l2

REAL FUNCTION m2(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    REAL,EXTERNAL::l1,m1
    m2 = cons(2)*(y+(cons(6)*l1(x,y,z,cons)/2))-cons(3)*(z+(cons(6)*m1(x,y,z,cons)/2))
END FUNCTION m2

REAL FUNCTION k3(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    REAL, EXTERNAl::k2,l2
    REAL :: k31, k32
    k31=cons(4)- cons(3)*(x+(cons(6)*k2(x,y,z,cons)/2))
    k32=(cons(1)/cons(5))*(y+(cons(6)*l2(x,y,z,cons)/2))*(x+(cons(6)*k2(x,y,z,cons)/2))
    k3=k31-k32
END FUNCTION k3

REAL FUNCTION l3(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    REAL, EXTERNAL::l2,k2
    REAL :: l31, l32
    l31=(cons(1)/cons(5))*(y+(cons(6)*l2(x,y,z,cons)/2))*(x+(cons(6)*k2(x,y,z,cons)/2))
    l32=(cons(3)+cons(2))*(y+(cons(6)*l2(x,y,z,cons)/2))
    l3 =l31-l32
END FUNCTION l3

REAL FUNCTION m3(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    REAL, EXTERNAL::l2,m2
    m3 = cons(2)*(y+(cons(6)*l2(x,y,z,cons)/2))-cons(3)*(z+(cons(6)*m2(x,y,z,cons)/2))
END FUNCTION m3

REAL FUNCTION k4(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    REAL,EXTERNAL::k3,l3
    k4= cons(4)-cons(3)*(x+cons(6)*k3(x,y,z,cons))-(cons(1)/cons(5))*(y+cons(6)*l3(x,y,z,cons))*(x+(cons(6)*k3(x,y,z,cons)))
END FUNCTION k4

REAL FUNCTION l4(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    REAL, EXTERNAL::l3,k3
    l4= (cons(1)/cons(5))*(y+(cons(6)*l3(x,y,z,cons)))*(x+(cons(6)*k3(x,y,z,cons)))-(cons(3)+cons(2))*(y+cons(6)*l3(x,y,z,cons))
END FUNCTION l4

REAL FUNCTION m4(x,y,z,cons)
    REAL, DIMENSION(7) :: cons
    REAL::x,y,z
    REAL, EXTERNAL::l3,m3
    m4= cons(2)*(y+cons(6)*l3(x,y,z,cons))-cons(3)*(z+cons(6)*m3(x,y,z,cons))
END FUNCTION m4