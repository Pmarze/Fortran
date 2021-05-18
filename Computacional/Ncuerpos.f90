!    2021-05-09
!    Ncuerpos.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o Ncuerpos.o Ncuerpos.f90
!    gfortran -o Ncuerpos.x Ncuerpos.o
!    ./Ncuerpos.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./Ncuerpos.x

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
!    along with this program.  IFnot, see
!    <http://www.gnu.org/licenses/>.

!    Copyright (C) 2021
!    B. E. Marroquín Morazán
!    bryant.morazan@gmail.com
!    Se tomó como base el programa de  Bryant Morazán, este fue modificado
!    y se reorganizó el flujo del programa donde se consideró necesario
!    para obtner un mejor funcionamiento y un código más ordenado.
!    Estos cambios fueron notificados y autorizados por el autor.

PROGRAM Ncuerpos_e
IMPLICIT NONE
    INTEGER :: caso=2           ! 1 electro, 2 gravedad
    INTEGER :: vec1, vec2
    ! Eje x
    REAL(16), DIMENSION(:), ALLOCATABLE :: x0, x !Posiciones 
    REAL(16), DIMENSION(:), ALLOCATABLE :: vx0, vx !Velocidades
    REAL(16), DIMENSION(:), ALLOCATABLE :: ax !Aceleraciones
    ! Eje y
    REAL(16), DIMENSION(:), ALLOCATABLE :: y0, y !Posiciones
    REAL(16), DIMENSION(:), ALLOCATABLE :: vy0, vy !Velocidades 
    REAL(16), DIMENSION(:), ALLOCATABLE :: ay !Aceleraciones

    REAL(16), DIMENSION(:), ALLOCATABLE :: q, m !Masas y cargas    
    REAL(16), DIMENSION(:), ALLOCATABLE :: r !Distancia    
    REAL(16), PARAMETER :: G = 6.674E-11
    REAL(16), PARAMETER :: k = 8.9874E+9
    REAL(16) :: mom_ang    
    REAL(16) :: t !Intervalo de tiempo
    INTEGER :: iteraciones, i, j, a, b, c
    IF(caso==1)THEN
        vec1=4
        vec2=6
    ELSE IF(caso==2)THEN
        vec1=3
        vec2=3
    END IF

    ALLOCATE(x0(vec1))
    ALLOCATE(x(vec1))
    ALLOCATE(vx0(vec1))
    ALLOCATE(vx(vec1))
    ALLOCATE(ax(vec1))
    ALLOCATE(y0(vec1))
    ALLOCATE(y(vec1))
    ALLOCATE(vy0(vec1))
    ALLOCATE(vy(vec1))
    ALLOCATE(ay(vec1))
    ALLOCATE(q(vec1))
    ALLOCATE(m(vec1))
    ALLOCATE(r(vec2))

    IF(caso==1)THEN
        m=(/9.11E-31,9.11E-31,9.11E-31,9.11E-31/)
        q=(/1.602E-19,1.602E-19,1.602E-19,1.602E-19/)
        x0=(/0.0, 5.0, 0.0, 5.0/)
        y0=(/0.0, 0.0, 1.0, 1.0/)
        vx0=(/0.0, 0.0, 0.0, 0.0/)
        vy0=(/0.0, 0.0, 0.0, 0.0/)
        OPEN(1, file='movimiento_2.txt')
        OPEN(2, file='momentum_angular_2.txt')        
    
    ELSE IF(caso==2)THEN
        ! Sol, Tierra, Luna
        m=(/1.989*10.0**30.0, 5.972*10.0**24.0, 7.349*10.0**22.0/)
        x0=(/0.0, 152098290.0, 152504990.0/)
        y0=(/0.0, 0.0, 0.0/)
        vx0=(/0.0, 0.0, 0.0/)
        vy0=(/0.0, 28.76, 29.72/)
        OPEN(1, file='tierra_2.txt')
        OPEN(4, file='tierra2_2.txt')
        OPEN(5, file='tierra3_2.txt')
        OPEN(6, file='tierra4_2.txt')
        OPEN(2, file='tierra-luna_2.txt')
        OPEN(3, file='momentum_angular_2.txt')        
    END IF
    t=20
    iteraciones=3000000
    DO i=0, iteraciones 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF(caso==1)THEN
        !-----------------------------Distancias------------------------------------- 
            r(1) = ( (x0(1) - x0(2))**2 + (y0(1) - y0(2))**2 )**0.5 !Entre 1 y 2
            r(2) = ( (x0(1) - x0(3))**2 + (y0(1) - y0(3))**2 )**0.5 !Entre 1 y 3
            r(3) = ( (x0(1) - x0(4))**2 + (y0(1) - y0(4))**2 )**0.5 !Entre 1 y 4
            r(4) = ( (x0(2) - x0(3))**2 + (y0(2) - y0(3))**2 )**0.5 !Entre 2 y 3
            r(5) = ( (x0(2) - x0(4))**2 + (y0(2) - y0(4))**2 )**0.5 !Entre 2 y 4
            r(6) = ( (x0(3) - x0(4))**2 + (y0(3) - y0(4))**2 )**0.5 !Entre 3 y 4
        !----------------------------Aceleraciones-----------------------------------
        !-------------Eje x-----------------
            ax(1)=(x0(1)-x0(2))*((k*q(1)*q(2))/m(1)-G*m(2))/r(1)**3+(x0(1)-x0(3))*((k*q(1)*q(3))/m(1)&
            &-G*m(3))/r(2)**3+(x0(1)-x0(4))*((k*q(1)*q(4))/m(1)-G*m(4))/r(3)**3
            ax(2) = (x0(2) - x0(1))*( (k*q(2)*q(1))/m(2) - G*m(1) )/r(1)**3 + (x0(2) - x0(3))*( (k*q(2)*q(3))/m(2)&
            & - G*m(3) )/r(4)**3 + (x0(2) - x0(4))*( (k*q(2)*q(4))/m(2) - G*m(4) )/r(5)**3
            ax(3) = (x0(3) - x0(1))*( (k*q(3)*q(1))/m(3) - G*m(1) )/r(2)**3 + (x0(3) - x0(2))*( (k*q(3)*q(2))/m(3)&
            & - G*m(2) )/r(4)**3 + (x0(3) - x0(4))*( (k*q(3)*q(4))/m(3) - G*m(4) )/r(6)**3
            ax(4) = (x0(4) - x0(1))*( (k*q(4)*q(1))/m(4) - G*m(1) )/r(3)**3 + (x0(4) - x0(2))*( (k*q(4)*q(2))/m(4)&
            & - G*m(2) )/r(5)**3 + (x0(4) - x0(3))*( (k*q(4)*q(3))/m(4) - G*m(3) )/r(6)**3 
        !-------------Eje y-----------------
            ay(1) = (y0(1) - y0(2))*( (k*q(1)*q(2))/m(1) - G*m(2) )/r(1)**3 + (y0(1) - y0(3))*( (k*q(1)*q(3))/m(1)&
            & - G*m(3) )/r(2)**3 + (y0(1) - y0(4))*( (k*q(1)*q(4))/m(1) - G*m(4) )/r(3)**3
            ay(2) = (y0(2) - y0(1))*( (k*q(2)*q(1))/m(2) - G*m(1) )/r(1)**3 + (y0(2) - y0(3))*( (k*q(2)*q(3))/m(2)&
            & - G*m(3) )/r(4)**3 + (y0(2) - y0(4))*( (k*q(2)*q(4))/m(2) - G*m(4) )/r(5)**3
            ay(3) = (y0(3) - y0(1))*( (k*q(3)*q(1))/m(3) - G*m(1) )/r(2)**3 + (y0(3) - y0(2))*( (k*q(3)*q(2))/m(3)&
            & - G*m(2) )/r(4)**3 + (y0(3) - y0(4))*( (k*q(3)*q(4))/m(3) - G*m(4) )/r(6)**3
            ay(4) = (y0(4) - y0(1))*( (k*q(4)*q(1))/m(4) - G*m(1) )/r(3)**3 + (y0(4) - y0(2))*( (k*q(4)*q(2))/m(4)&
            & - G*m(2) )/r(5)**3 + (y0(4) - y0(3))*( (k*q(4)*q(3))/m(4) - G*m(3) )/r(6)**3
        ELSE IF(caso==2)THEN
        !-----------------------------Distancias------------------------------------- 
            r(1) = ( (x0(1) - x0(2))**2 + (y0(1) - y0(2))**2 )**0.5 !Entre 1 y 2
            r(2) = ( (x0(1) - x0(3))**2 + (y0(1) - y0(3))**2 )**0.5 !Entre 1 y 3
            r(3) = ( (x0(2) - x0(3))**2 + (y0(2) - y0(3))**2 )**0.5 !Entre 2 y 3
        !----------------------------Aceleraciones-----------------------------------
        !-------------Eje x-----------------
            ax(1) = -G*( (x0(1) - x0(2))*m(2)/r(1)**3 + (x0(1) - x0(3))*m(3)/r(2)**3 ) 
            ax(2) = -G*( (x0(2) - x0(1))*m(1)/r(1)**3 + (x0(2) - x0(3))*m(3)/r(3)**3 ) 
            ax(3) = -G*( (x0(3) - x0(1))*m(1)/r(2)**3 + (x0(3) - x0(2))*m(2)/r(3)**3 ) 
        !-------------Eje y-----------------
            ay(1) = -G*( (y0(1) - y0(2))*m(2)/r(1)**3 + (y0(1) - y0(3))*m(3)/r(2)**3 ) 
            ay(2) = -G*( (y0(2) - y0(1))*m(1)/r(1)**3 + (y0(2) - y0(3))*m(3)/r(3)**3 ) 
            ay(3) = -G*( (y0(3) - y0(1))*m(1)/r(2)**3 + (y0(3) - y0(2))*m(2)/r(3)**3 )
        END IF
        !---------------------------Velocidades---------------------------------------
        !-----------Eje x-------------------
            vx = vx0 + t*ax
        !-----------Eje y-------------------
            vy = vy0 + t*ay
        !-----------------------------------------------------------------------------
        !------------------------------Posiciones-------------------------------------
        !-------------Eje x---------------
            x = x0 + t*vx0 + (t**2)*ax/2
        !-------------Eje y---------------
            y = y0 + t*vy0 + (t**2)*ay/2
        !-----------------------------------------------------------------------------
        !-------------------------Reasignacion de valores-----------------------------
        !-----------Eje x--------------
            x0 = x
            vx0 = vx
        !-----------Eje y--------------
            y0 = y
            vy0 = vy
        !-----------------------------------------------------------------------------
        !---------------------------Momentum angular----------------------------------
        IF(caso==1)THEN
            mom_ang = m(1)*( x0(1)*vy0(1) - y(1)*vx0(1) ) + m(2)*( x0(2)*vy0(2) - y(2)*vx0(2) ) +&
            & m(3)*( x0(3)*vy0(3) - y(3)*vx0(3) )+ m(4)*( x0(4)*vy0(4) - y(4)*vx0(4) )        
        !-------------------------Escritura de resultados-----------------------------
        !-------------Trayectorias-----------
        !----------Momentum angular----------
            WRITE(2,*) mom_ang
        !-------------------------------------        
                WRITE(1,*) x(1),';',y(1),';',x(2),';',y(2),';', x(3),';',y(3),';', x(4),';',y(4)
            ELSE IF(caso==2)THEN    
            mom_ang = m(1)*(x0(1)*vy0(1)-y(1)*vx0(1))+m(2)*(x0(2)*vy0(2)-y(2)*vx0(2))+m(3)*(x0(3)*vy0(3)-y(3)*vx0(3))
        !-------------------------Escritura de resultados-----------------------------
        !---------Orbita terrestre-----------
            WRITE(1,*) -x(2), y(2)
            WRITE(4,*) x(2), y(2)
            WRITE(5,*) -x(2), -y(2)
            WRITE(6,*) x(2), -y(2)
        !-----Orbita lunar y terrestre-------
                WRITE(2,*)x0(1),';',y0(1),';',x0(2),';',y0(2),';', x0(3)+70*(x0(3)-x0(2)),';',y0(3)+70*(y0(3)-y0(2))
        !----------Momentum angular----------
            WRITE(3,*) mom_ang
        !-------------------------------------        
        END IF
        IF(caso==1)THEN
            CLOSE(1)
            CLOSE(2)
        ELSE IF(caso==2)THEN
            CLOSE(1)
            CLOSE(2)
            CLOSE(3)
            CLOSE(4)
            CLOSE(5)
            CLOSE(6)                
        END IF
    END DO
END PROGRAM Ncuerpos_e