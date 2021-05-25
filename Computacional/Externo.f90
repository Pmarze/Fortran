!    2021-05-24
!    Externo.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o Externo.o Externo.f90
!    gfortran -o Externo.x Externo.o
!    ./Externo.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./Externo.x

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

PROGRAM Externo
IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Columnas y filas
    INTEGER,PARAMETER :: col_param=4, fil_param=6
    INTEGER :: col_Matri, fil_Matri, Tam_MatG
    REAL, DIMENSION(:,:), allocatable :: Parametros, Matriz, MatGau, XGau
    INTEGER :: i, j, k, l, iprima
    REAL :: a
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    ! Arreglos
    ALLOCATE(Parametros(col_param,fil_param))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    ! Archivos
    OPEN(10, file="parametros.txt",status="old")        ! Se con los parámetros de la ecuación
        READ(10,*) ((Parametros(k,l),k=1,col_param),l=1,fil_param) ! Se almacena en la matriz
    CLOSE(10) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    a=Parametros(1,1)
    IF (a==1)THEN
        col_matri=INT(Parametros(1,2))        ! nx
        fil_matri=INT(Parametros(2,2))        ! ny
    ELSE IF (a==2)THEN
        col_matri=INT(Parametros(1,2))        ! nx
        fil_matri=INT(Parametros(4,2))        ! nt
    ELSE IF (a==3)THEN
        col_matri=INT(Parametros(1,2))        ! nx
        fil_matri=INT(Parametros(4,2))        ! nt       
    END IF    
    Tam_MatG=(col_matri-2)*(fil_matri-2)
    ALLOCATE(Matriz(col_matri,fil_matri))      
    ALLOCATE(XGau(1,Tam_MatG))
    ALLOCATE(MatGau(Tam_MatG,Tam_MatG))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    OPEN(11, file="Gauss_Frontera.txt",status="old")        
        READ(11,*) ((Matriz(k,l),k=1,col_param),l=1,fil_param) ! Se almacena en la matriz
    CLOSE(11)


    OPEN(11, file="Gauss_Result.txt",status="old")        ! Resultados de diagonalizar la matriz
        READ(11,*) (XGau(1,l),k=1,col_param)              ! se almacena en un vector
    CLOSE(11)
    
    ! Se rellena la matriz con los valores calculados en el programa externo
    DO j=2, fil_matri-1
        DO k=2, col_matri-1
            iprima=(j-1)*(col_matri-2)+k-4
            Matriz(k,j)=XGau(1,iprima)
        END DO
    END DO
    ! Se exportan los resultados con el valor de cada punto
    OPEN(12, file='Resultados_Externo.txt')
        DO i=1, fil_matri
            WRITE(12, '(1000F14.7)')( real(Matriz(i,j)) ,j=1,col_matri)
        END DO
    CLOSE(12)

END PROGRAM Externo