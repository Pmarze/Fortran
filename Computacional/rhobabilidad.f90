!    2021-03-31
!    rhobabilidad.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que genera una distribución de números aleatorios de probabilidad
!    uniforme y la transforma a una distribución normal y a una exponencial, 
!    según la elección del usuario, los datos obtenidos se guardan en un archivo
!    de texto y se pueden ver o no en la terminal, según desee el usuario.

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o rhobabilidad.o rhobabilidad.f90
!    gfortran -o rhobabilidad.x rhobabilidad.o
!    gfortran ./rhobabilidad.x

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

PROGRAM rhobabilidad
    IMPLICIT NONE
    INTEGER, PARAMETER :: n=1000                ! Cantidad de datos aleatorios
    REAL, DIMENSION(n) :: r,y,z                 ! arreglo para cada distribucion
    REAL :: delta=1                             ! delta, distribución exponencial
    INTEGER(2) :: i                             ! variable de conteo
    INTEGER(2) :: caso=1, impr=1                ! caso 1 = normal , caso 2= exponencial
    ! si impr=1 los datos obtenidos se muestran en la terminal
    CALL random_number(r)                       ! se rellena el arreglo de tamaño n con
                                                ! números aleatorios entre 0 y 1
    IF (caso==1)THEN                            ! Si caso=1 --> distribución normal
        OPEN(12,file='rho_Normal.txt')          ! Se genera un documento de texto
        WRITE(12,*) 'Distribución normal'
        WRITE(12,*) 'Dist. Uniforme   , Dist. Normal'
        DO i=1, n                               ! Cada coordenada se transforma según
            CALL normal(r(i),y(i))              ! la funcion normal y se almacena en y(i)
            WRITE(12,*) r(i),',',y(i)
        END DO
        IF (impr==1)THEN                        ! Se imprime en pantalla si se solicitó
            print*, 'Distribución normal'
            print*, y
        END IF
    ELSE IF (caso==2)THEN                       ! Si caso=2 --> distrubución exponencial
        OPEN(12,file='rho_Exponencial.txt')
        WRITE(12,*) 'Distribución exponencial, delta=',delta
        WRITE(12,*) 'Dist. Uniforme   , Dist. Exponencial'
        DO i=1, n                               ! Cada coordenada se transforma según
            CALL expone(r(i),delta,z(i))        ! la función expone y se almacena en z(i)
            WRITE(12,*) r(i),',',z(i)
        END DO
        IF (impr==1)THEN                        ! Se imprime en pantalla si se solicitó
            print*, 'Distribución exponencial'
            print*, z
        END IF            
    END IF
END PROGRAM rhobabilidad

SUBROUTINE normal(a,y)                          ! Función que transforma una dist. uniforme
    REAL, INTENT(IN) :: a                       ! en una dist. normal
    REAL, INTENT(OUT) :: y
    y=SQRT(LOG(1/a**2))                         ! Ecuación dada por el método inverso
END SUBROUTINE normal

SUBROUTINE expone(a,n,y)                        ! Funcione que transforma una dist. uniforme
    REAL, INTENT(IN) :: a, n                    ! en una dist. exponencial
    REAL, INTENT(OUT) :: y
    y=(-1/n)*LOG(a)                             ! Ecuación dada por el método inverso
END SUBROUTINE expone