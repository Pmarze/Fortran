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
    INTEGER(2) :: caso=2                        ! caso 1 = normal , caso 2= exponencial
    REAL :: delta=1                             ! delta, distribución exponencial
    REAL, DIMENSION(n) :: r,y,z                 ! arreglo para cada distribucion
    INTEGER :: rank=1                           ! Canal de comunicación del archivo .txt
    ! si impr=1 los datos obtenidos se muestran en la terminal
    CALL random_number(r)       ! se rellena el arreglo con números aleatorios entre 0 y 1
    IF (caso==1)THEN                            ! Si caso=1 --> distribución normal
        OPEN(rank, file='rho_Normal.txt')       ! Se genera un documento de texto
        WRITE(rank,*) 'Dist. Uniforme   , Dist. Normal'
        CALL normal(r,n,rank,y)                 ! Función dist. normal
    ELSE IF (caso==2)THEN                       ! Si caso=2 --> distrubución exponencial
        OPEN(rank, file='rho_Exponencial.txt')  ! Se genera un documento de texto
        WRITE(rank,*) 'Dist. Uniforme   , Dist. Exponencial','delta=',delta
        CALL expone(r,delta,n,rank,z)           ! Función dist. normal
    END IF
END PROGRAM rhobabilidad

SUBROUTINE normal(a,n,rank,y)                   ! Función que transforma una dist. uniforme
    INTEGER :: n, i, rank                       ! Variables de la función
    REAL, DIMENSION(n), INTENT(IN) :: a         ! en una dist. normal
    REAL, DIMENSION(n), INTENT(OUT) :: y
    DO i=1, n
        y(i)=SQRT(-LOG(a(i)**2))                ! Ecuación dada por el método inverso
        WRITE(rank,*) a(i),',',y(i)             ! Se almacena el resultado
    END DO
END SUBROUTINE normal

SUBROUTINE expone(a,b,n,rank,y)                 ! Funcione que transforma una dist. uniforme
    REAL :: b
    INTEGER :: n, i, rank
    REAL, DIMENSION(n), INTENT(IN) :: a         ! en una dist. exponencial
    REAL, DIMENSION(n), INTENT(OUT) :: y
    DO i=1, n
        y(i)=(-1/b)*LOG(a(i))                   ! Ecuación dada por el método inverso
        WRITE(rank,*) a(i),',',y(i)             ! Se almacena el resultado
    END DO
END SUBROUTINE expone