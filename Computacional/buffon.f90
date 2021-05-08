!    2020-03-14
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que 


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o buffon.o buffon.f90
!    gfortran -o buffon.x buffon.o
!    ./buffon.x

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
PROGRAM buffon
IMPLICIT NONE
    INTEGER :: n=1e9, a=1e6          ! Cantidad de iteraciones, para análisis de p_pi(n)
    INTEGER :: i, j=0                ! Índice de sumatoria, cantidad de aciertos
    REAL :: x0, xf, t0               ! Punta aguja, Otro extremo, ángulo de la aguja
    REAL :: d, r                     ! Distancia entre líneas, tamaño de la aguja
    REAL :: p, p_pi                  ! Probabilidad, Pi calculado por probabilidad
    REAL,PARAMETER :: PI=3.141592653238 ! Valor de PI
    OPEN(12,file='Buf.txt')             ! Abre y/o crea el archivo de texto para guardar los datos
    d=1                                 ! Distancia entre las dos líneas de separación
    r=1                                 ! tamaño de la aguja considerando r<1 
    DO i=1, n
        CALL RANDOM_NUMBER(x0)          ! Posición inicial aleatoria
        CALL RANDOM_NUMBER(t0)          ! Theta inicial aleatoria
        x0=x0*r                         ! Una punta de la aguja siempre cae entre 0 y d
        t0=t0*2*PI                      ! El ángulo en el que apunta la aguja está entre 0 y 2π
        xf=x0+r*COS(t0)                 ! por identidades trigonométricas x0+r*cos(θ)=x
        IF (xf<=0 .or. xf>=1)THEN       ! Si esto pasa es porque la aguja cruzó la línea
            j=j+1                       ! Cantidad de aciertos +1
        END IF  
        IF(MOD(i,a)==0)THEN
            p=REAL(j)/REAL(i)               ! P=cantidad de aciertos/total
            p_pi=(2*r)/p                    ! Pi calculado por probabilidad = 2*r/p    
            WRITE(12,*) i, p_pi
        END IF    
    END DO
    PRINT*, p_pi, PI                    ! Se muestra el último valor obtenido con el esperado
END PROGRAM buffon