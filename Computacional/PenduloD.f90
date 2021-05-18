!    2021-05-17
!    PenduloD.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o PenduloD.o PenduloD.f90
!    gfortran -o PenduloD.x PenduloD.o
!    ./PenduloD.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./PenduloD.x

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

PROGRAM PenduloD 
IMPLICIT NONE
    !Constantes/propiedades de los péndulos
    REAL,PARAMETER :: l1=10.00, l2=15.00
    REAL,PARAMETER :: m1=15.00, m2=3.00
    REAL,PARAMETER :: g=9.80991
    !Tasa de muestreo
    REAL, PARAMETER :: h=0.01
    REAL, PARAMETER :: tfinal=50.00
    INTEGER :: N, i
    !CONDICIONES INICIALES
    REAL, PARAMETER :: phi10= 0.0, phi20= 0.2
    REAL, PARAMETER :: omega10= 0.5, omega20= 0.0
    REAL, DIMENSION(:), ALLOCATABLE :: o, p, q, r
    REAL, DIMENSION(5) :: cons=(/g,m1,m2,l1,l2/)
    !Coeficientes para el método de runge-kutta
    REAL:: k1phi1,k1phi2,k1omega1,k1omega2,k2phi1,k2phi2,k2omega1,k2omega2
    REAL:: k3phi1,k3phi2,k3omega1,k3omega2, k4phi1,k4phi2,k4omega1,k4omega2  
    !Se llaman a las funciones 
    REAL :: omega1prim,omega2prim, t=0       
    N=CEILING(tfinal/h)
    ALLOCATE(o(N+1))
    ALLOCATE(p(N+1))
    ALLOCATE(q(N+1))
    ALLOCATE(r(N+1))
    o(1)=phi10
    p(1)=phi20
    q(1)=omega10
    r(1)=omega20    
    OPEN(12,file="pendulo_doble.txt")
        DO i=1,N
            !k1's
            k1phi1 = q(i)
            k1phi2 = r(i)
            k1omega1 = omega1prim(o(i),p(i),r(i),cons)
            k1omega2 = omega2prim(o(i),p(i),q(i),r(i),cons)
            !k2's
            k2phi1=q(i)+(h/2)*k1omega1
            k2phi2=r(i)+(h/2)*k1omega2
            k2omega1=omega1prim(o(i)+(h/2)*k1phi1,p(i)+(h/2)*k1phi2, r(i)+(h/2)*k1omega2,cons)
            k2omega2=omega2prim(o(i)+(h/2)*k1phi1,p(i)+(h/2)*k1phi2, q(i)+(h/2)*k1omega1 ,r(i)+(h/2)*k1omega2,cons)
            !k3's
            k3phi1=q(i)+(h/2)*k2omega1
            k3phi2=r(i)+(h/2)*k2omega2
            k3omega1=omega1prim(o(i)+(h/2)*k2phi1, p(i)+(h/2)*k2phi2 , r(i)+(h/2)*k2omega2,cons)
            k3omega2=omega2prim(o(i)+(h/2)*k2phi1, p(i)+(h/2)*k2phi2 , q(i)+(h/2)*k2omega1 , r(i)+(h/2)*k2omega2,cons)
            !k4's
            k4phi1=q(i)+h*k3omega1
            k4phi2=r(i)+h*k3omega2
            k4omega1=omega1prim(o(i)+h*k3phi1,p(i)+h*k3phi2,r(i)+h*k3omega2,cons)
            k4omega2=omega2prim(o(i)+h*k3phi1,p(i)+h*k3phi2,q(i)+h*k3omega1,r(i)+h*k3omega2,cons)
            WRITE(12,*) t,";",o(i),";",p(i),";",q(i),";",r(i)
            o(i+1)=o(i)+(h/6)*(k1phi1+2*k2phi1+2*k3phi1+k4phi1)
            p(i+1)=p(i)+(h/6)*(k1phi2+2*k2phi2+2*k3phi2+k4phi2)
            q(i+1)=q(i)+(h/6)*(k1omega1+2*k2omega1+2*k3omega1+k4omega1)
            r(i+1)=r(i)+(h/6)*(k1omega2+2*k2omega2+2*k3omega2+k4omega2)
            t=t+h
        END DO
     CLOSE(7)     
END PROGRAM PenduloD

REAL FUNCTION omega1prim(phi1,phi2,omega2,cons)
    REAL,DIMENSION(5), INTENT(IN) :: cons 
    REAL,INTENT(IN) :: phi1,phi2,omega2
    omega1prim= (-cons(1)*(2*cons(2)+cons(3))*sin(phi1)+cons(3)*cons(1)*sin(phi1-2*phi2)&
    &-2*sin(phi1-phi2)*cons(3)*(omega2**2*cons(5)+omega2**2*cons(4)*cos(phi1-phi2)))&
    &/(cons(4)*2*(cons(2)+cons(3)-cons(3)*cos(2*phi1-2*phi2)))
END FUNCTION omega1prim

REAL FUNCTION omega2prim(phi1,phi2,omega1,omega2,cons)
    REAL,DIMENSION(5), INTENT(IN) :: cons 
    REAL,INTENT(IN)::phi1,phi2,omega1,omega2
    omega2prim  =(2*sin(phi1-phi2)*(omega1**2*cons(4)*(cons(2)+cons(3))+cons(1)*(cons(2)+cons(3))*cos(phi1)&
    &+omega2**2*cons(5)*cons(3)*cos(phi1-phi2)))/(cons(5)*(2*cons(2)+cons(3)-cons(3)*cos(2*phi1-2*phi2)))
END FUNCTION omega2prim