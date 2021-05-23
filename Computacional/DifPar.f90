!    2021-05-21
!    DifPar.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o DifPar.o DifPar.f90
!    gfortran -o DifPar.x DifPar.o
!    ./DifPar.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./DifPar.x

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

SUBROUTINE U(x,y,z,t,uxyzt)
    REAL, INTENT(IN) :: x, y, z, t
    REAL, INTENT(OUT) :: uxyzt
    uxyzt=x*y*z*t
END SUBROUTINE U


PROGRAM DifPar
IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Columnas y filas
    INTEGER,PARAMETER :: col_param=4, fil_param=6
    INTEGER :: col_Matri, fil_Matri, Tam_MatG
    REAL, DIMENSION(:,:), allocatable :: Parametros, Matriz, MatGau, XGau
    REAL, DIMENSION(:,:), allocatable :: Xanterior, Xposterior, errseidel
    REAL :: sumseidel, tolerseidel
    INTEGER :: iteraseidel=20
    INTEGER :: i, j, k, l, iprima
    REAL :: delta_x, delta_y, delta_t
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    ! Arreglos
    ALLOCATE(Parametros(col_param,fil_param))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    ! Archivos
    OPEN(10, file="parametros.txt",status="old")        ! Se con los parámetros de la ecuación
        READ(10,*) ((Parametros(k,l),k=1,col_param),l=1,fil_param) ! Se almacena en la matriz
    CLOSE(10) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IF (Parametros(1,1)==1)THEN
        col_matri=INT(Parametros(1,2))        ! nx
        fil_matri=INT(Parametros(2,2))        ! ny
        delta_x=ABS(Parametros(2,3)-Parametros(1,3))/INT(Parametros(1,2))
        delta_y=ABS(Parametros(4,3)-Parametros(3,3))/INT(Parametros(2,2))
    ELSE IF (Parametros(1,1)==2)THEN
        col_matri=INT(Parametros(1,2))        ! nx
        fil_matri=INT(Parametros(4,2))        ! nt
        delta_x=ABS(Parametros(2,3)-Parametros(1,3))/INT(Parametros(1,2))
        delta_t=ABS(Parametros(4,4)-Parametros(3,4))/INT(Parametros(4,2))
    ELSE IF (Parametros(1,1)==3)THEN
        col_matri=INT(Parametros(1,2))        ! nx
        fil_matri=INT(Parametros(4,2))        ! nt
        delta_x=ABS(Parametros(2,3)-Parametros(1,3))/INT(Parametros(1,2))
        delta_t=ABS(Parametros(4,4)-Parametros(3,4))/INT(Parametros(4,2))        
    END IF    
    ALLOCATE(Matriz(col_matri,fil_matri))      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Condiciones de frontera de la matriz
    DO i=1, col_matri
        Matriz(1,i)=Parametros(1,5)             ! Lado izquierdo 
        Matriz(col_matri,i)=Parametros(2,5)     ! Lado derecho
        Matriz(i,1)=Parametros(4,5)             ! Lado superior
        Matriz(i,col_matri)=Parametros(3,5)     ! Lado inferior        
    END DO
    
    IF (Matriz(1,2)>Matriz(2,1))THEN
        Matriz(1,1)=Matriz(2,1)+((Matriz(1,2)-Matriz(2,1))/2)
    ELSE IF (Matriz(1,2)<Matriz(2,1))THEN
        Matriz(1,1)=Matriz(1,2)+(Matriz(2,1)-(Matriz(1,2))/2)
    END IF

    IF (Matriz(1,fil_matri-1)>Matriz(2,fil_matri))THEN
        Matriz(1,fil_matri)=Matriz(2,fil_matri)+((Matriz(1,fil_matri-1)-Matriz(2,fil_matri))/2)
    ELSE IF (Matriz(1,fil_matri-1)<Matriz(2,fil_matri))THEN
        Matriz(1,fil_matri)=Matriz(1,fil_matri-1)+(Matriz(2,fil_matri)-(Matriz(1,fil_matri-1))/2)
    END IF

    IF (Matriz(col_matri-1,1)>Matriz(col_matri,2))THEN
        Matriz(col_matri,1)=Matriz(col_matri,2)+((Matriz(col_matri-1,1)-Matriz(col_matri,2))/2)
    ELSE IF (Matriz(1,col_matri-1)<Matriz(2,col_matri))THEN
        Matriz(col_matri,1)=Matriz(col_matri-1,2)+(Matriz(col_matri,2)-(Matriz(col_matri-1,1))/2)
    END IF
    
    IF (Matriz(col_matri,fil_matri-1)>Matriz(col_matri-1,fil_matri))THEN
        Matriz(col_matri,fil_matri)=Matriz(col_matri-1,fil_matri)+((Matriz(col_matri,fil_matri-1)-Matriz(col_matri-1,fil_matri))/2)
    ELSE IF (Matriz(col_matri,fil_matri-1)<Matriz(col_matri-1,fil_matri))THEN
        Matriz(col_matri,fil_matri)=Matriz(col_matri,fil_matri-1)+(Matriz(col_matri-1,fil_matri)-(Matriz(col_matri,fil_matri-1))/2)
    END IF    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Matriz para diagonalizar
    Tam_MatG=(col_matri-2)*(fil_matri-2) ! una matriz dentro de otra tiene m-2 n-2
    ALLOCATE(MatGau(Tam_MatG,Tam_MatG))        
    DO i=1,Tam_MatG
        DO j=1, fil_matri-2
            DO k=0, col_matri-2-1
                iprima=(j-1)*(col_matri-2)+1+k
                IF (iprima==i)THEN                                          ! P_{i,j}
                    MatGau(iprima,i)=-2*((1/delta_x**2)+(1/delta_y**2))
                ELSE IF (iprima==i-1 .and. MOD(i-1,col_matri-2)/=0)THEN     ! P_{i-1,j}
                    MatGau(iprima,i)=1/delta_x**2
                ELSE IF (iprima==i+1 .and. MOD(i+1-1,col_matri-2)/=0)THEN   ! P_{i+1,j}
                    MatGau(iprima,i)=1/delta_x**2
                ELSE IF (iprima==i+col_matri-2)THEN                         ! P_{i,j+1}
                    MatGau(iprima,i)=1/delta_y**2
                ELSE IF (iprima==i-col_matri+2)THEN                         ! P_{i,j-1}
                    MatGau(iprima,i)=1/delta_y**2
                ELSE 
                    MatGau(iprima,i)=0
                END IF
            END DO
        END DO
    END DO                    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Vector de Puntos P(i)    
    ALLOCATE(XGau(1,Tam_MatG))
    XGau=0
    XGau(1,1)=Matriz(1,2)+Matriz(2,1)
    XGau(1,col_matri-2)=Matriz(fil_matri-1,1)+Matriz(col_matri,2)
    XGau(1,Tam_MatG)=Matriz(col_matri-1,fil_matri)+Matriz(col_matri,fil_matri-1)   
    XGau(1,Tam_MatG-col_matri+3)=Matriz(1,fil_matri-1)+Matriz(1,2)
    
    DO i=3,col_matri-2
        XGau(1,i-1)=Matriz(i,1)
    END DO
    DO i=3,col_matri-2
        XGau(1,Tam_MatG-col_matri+i+1)=Matriz(2,fil_matri)
    END DO
    DO i=3, fil_matri-2
        XGau(1,(col_matri-2)*(i-2)+1)=Matriz(1,i)
    END DO
    DO i=3, fil_matri-2
        XGau(1,(col_matri-2)*(i-2)+col_matri-2)=Matriz(col_matri,i)
    END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Gauss Seidel
    ALLOCATE(Xanterior(1,Tam_MatG))
    ALLOCATE(Xposterior(1,Tam_MatG))
    Xanterior=0
    iteraseidel=100000
    DO k=1,iteraseidel
        DO i=1, Tam_MatG
            DO j=1, Tam_MatG
                IF (i==j)THEN
                ELSE 
                sumseidel=sumseidel+MatGau(j,i)*Xanterior(1,j)
                END IF
            END DO
            Xposterior(1,i)=(XGau(1,i)-sumseidel)/MatGau(i,i)
        END DO    
        !errseidel=ABS((Xposterior-Xanterior)/Xposterior)
    END DO
    !PRINT*, Xposterior
    
        DO j=2, fil_matri-1
            DO k=2, col_matri-1
                iprima=(j-1)*(col_matri-2)+k-4
                Matriz(k,j)=Xposterior(1,iprima)
            END DO
        END DO
    
    
    PRINT*, Matriz
    !PRINT*, ''
    !PRINT*, MatGau
    !PRINT*, ''
    !PRINT*, XGau
    !Print*, matriz

END PROGRAM DifPar

