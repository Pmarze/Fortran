!    2021-04-01
!    MontyH.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que 


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o MontyH.o MontyH.f90
!    gfortran -o MontyH.x MontyH.o
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./MontyH.x

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

PROGRAM MontyH
IMPLICIT NONE
    INTEGER :: n=100000000
    INTEGER :: posic, puert, decision, mostrar, cambio, resultado, i
    INTEGER :: deccamb=0, decnocamb=0
    INTEGER, DIMENSION(3) :: puerta, cabras, puertaf
    REAL :: a, b, c
    DO i=1, n
        resultado=0
        CALL random_number(a)
        CALL random_number(b)
        CALL random_number(c)
        puert=INT(a*3)
        posic=INT(b*3)
        decision=INT(c*2)
        CALL selpu(puert,puerta)
        !print*, 'posic', puerta
        CALL selca(posic,cabras)
        !print*, 'carro',cabras
        CALL mosca(puerta,cabras,mostrar)
        CALL flip(puerta,mostrar,decision,puertaf,cambio)
        !print*, 'posic', puertaf
        CALL anali(puertaf,cabras,resultado)
        !print*, 'resultado',resultado
        IF(cambio==0)THEN
            IF (resultado==1)THEN
                !print*, 'no cambió y ganó'
                decnocamb=decnocamb+1
            ELSE
                !print*, 'no cambió y perdió'
                deccamb=deccamb+1
            END IF
        ELSE
            IF(resultado==1)THEN
                !print*, 'cambió y ganó'
                deccamb=deccamb+1
            ELSE 
                !print*, 'cambió y perdió'
                decnocamb=decnocamb+1
            END IF
        END IF
    END DO
    print*, 'habia que cambiar=',deccamb,'no había que cambiar=',decnocamb
END PROGRAM MontyH

SUBROUTINE selpu(a,y)
    INTEGER, INTENT(IN) :: a
    INTEGER, DIMENSION(3), INTENT(OUT) :: y
    IF(a==0)THEN
        y=(/1,0,0/)
    ELSE IF(a==1)THEN
        y=(/0,1,0/)
    ELSE IF(a==2)THEN
        y=(/0,0,1/)
    END IF
END SUBROUTINE selpu

SUBROUTINE selca(a,y)
    INTEGER, INTENT(IN) :: a
    INTEGER, DIMENSION(3), INTENT(OUT) :: y
    IF(a==0)THEN
        y=(/1,0,0/)
    ELSE IF(a==1)THEN
        y=(/0,1,0/)
    ELSE IF(a==2)THEN
        y=(/0,0,1/)
    END IF
END SUBROUTINE selca

SUBROUTINE mosca(a,b,y)
    INTEGER, DIMENSION(3), INTENT(IN) :: a, b
    INTEGER, INTENT(OUT) :: y
    IF(a(1)==1)THEN
        IF(b(2)==0)THEN
            y=2
        ELSE
            y=3
        END IF
    ELSE IF(a(2)==1)THEN
        IF (b(1)==0)THEN
            y=1
        ELSE 
            y=3
        END IF
    ELSE IF(a(3)==1)THEN
        IF (b(1)==0)THEN
            y=1
        ELSE 
            y=2
        END IF
    END IF
END SUBROUTINE mosca

SUBROUTINE flip(a,b,c,y,z)
    INTEGER, DIMENSION(3), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: b,c
    INTEGER, DIMENSION(3), INTENT(OUT) :: y
    INTEGER, INTENT(OUT) :: z
    y=a
    IF(c==1)THEN
        z=1
        IF(b==1)THEN
            d=a(2)
            y(2)=a(3)
            y(3)=a(2)
        ELSE IF(b==2)THEN
            y(1)=a(3)
            y(3)=a(1)
        ELSE IF(b==3)THEN
            y(2)=a(1)
            y(1)=a(2)
        END IF
    ELSE
        z=0
    END IF
END SUBROUTINE flip

SUBROUTINE anali(a,b,y)
    INTEGER, DIMENSION(3), INTENT(IN) :: a,b
    INTEGER, INTENT(OUT) :: y
    INTEGER, DIMENSION(3) :: c
    INTEGER :: i
    c=a+b
    DO i=1, 3
        IF (c(i)==2)THEN
            y=1
        END IF
    END DO
END SUBROUTINE