!    2021-05-09
!    NumNewt.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o NumNewt.o NumNewt.f90
!    gfortran -o NumNewt.x NumNewt.o
!    ./NumNewt.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./NumNewt.x

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
!    Jose tecún
!    josetecun@gmail.com
!    La estructura para almacenar los archivos de texto en matrices
!    ordenadas por filas y columnas:
!       INTEGER :: id,jd
!       OPEN(13, file="....",status="old")
!       READ(13,*) ((Datos(id,jd),jd=1,col1),id=1,filas1)
!       CLOSE(13)
!    fue proporcionado por José Tecún y es utilizado bajo su consentimiento

PROGRAM NumNewt
    IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Columnas y filas
    INTEGER :: col1=2, filas1=6                 ! Número de filas y columna de los datos a fitear
    INTEGER :: col2=2, filas2=6                 ! Número de filas y columnas de los datos a comprobar
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    REAL(8) :: P, x_xj, x, err                  ! Definiciones de Polinomio de Newton P(x)
    REAL(8), DIMENSION(:,:), allocatable :: Datos, Difdiv, Multi, Datospru
    INTEGER :: i, j, k, l                       ! Indices de sumatoria
    INTEGER :: id,jd                            ! Indices para almacenar datos en matriz
    ALLOCATE(Datos(filas1,col1))                ! Matriz de datos de tamaño filas1*col1
    ALLOCATE(Difdiv(filas1-1,filas1-1))         ! Matriz para diferencias divididas
    ALLOCATE(Multi(filas1-1,1))                 ! Matriz para la multiplicación (x-x0)...(x-xj)
    ALLOCATE(Datospru(filas2,col2))             ! Matriz de datos a corroborar
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    ! Archivos
    OPEN(13, file="newtpy",status="old")        ! Se lee el archivo a utilizar
        READ(13,*) ((Datos(id,jd),jd=1,col1),id=1,filas1) ! Se almacena en la matriz
    CLOSE(13)
    OPEN(14, file="newtprupy",status="old")     ! Se lee el archivo a utilizar
        READ(14,*) ((Datospru(id,jd),jd=1,col2),id=1,filas2) ! Se almacena en la matriz
    CLOSE(14)    
!    Print*, Datos(1,1),Datos(1,2),Datos(1,3)   ! Corroborar que la lista se almacenó correctamente
!    Print*, Datospru(1,1),Datospru(1,2),Datospru(1,3)  ! Corroborar que la lista se almacenó correctamente    
    OPEN(12,file='Resultados_Newt1.txt')        ! Los resultados se almacenan en un archivo    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    DO i=1, filas1-1                            ! Se calculan las diferencias divididas f[xj,xi]
        Difdiv(i,1)=(Datos(i+1,2)-Datos(i,2))/(Datos(i+1,1)-Datos(i,1))
    END DO
    DO j=2, filas1-1                            ! Se calculan las dif. div de orden superior
        DO k=1, filas1-j
            Difdiv(k,j)=(Difdiv(k+1,j-1)-Difdiv(k,j-1))/(Datos(k+j,1)-Datos(k,1))
        END DO
    END DO
    DO l=1,filas2
        x=Datospru(l,1)                         ! Se lee el valor de x_l
        DO i=1, filas1
            Multi(i,1)=x-Datos(i,1)             ! Se calcula el producto (x-x0)...(x-xi)
        END DO
        P=Datos(1,2)                            ! a0
        P=P+Difdiv(1,1)*Multi(1,1)              ! a1(x-x0)
        DO j=2, filas1-1                        ! Se calculan los demás términos de la sumatoria
            x_xj=1                              ! Valor inicial de la multiplicatoria
            DO k=1, j
                x_xj=x_xj*Multi(k,1)
            END DO
            P=P+Difdiv(1,j)*x_xj                ! Se calcula el valor de P(x)
        END DO
        err=ABS(P-Datospru(l,2))                ! Se muestra el error absoluto para cada dato
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    ! Muestra de datos
        PRINT*, 'P=',P                          ! Se muestra en pantalla        
        PRINT*, 'error=',err
        WRITE(12,*) P,err                       ! Se almacena en el archivo el resultado con su error
    END DO      
    CLOSE(12)  
END PROGRAM NumNewt