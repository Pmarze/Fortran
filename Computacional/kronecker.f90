!    2020-03-14
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que 


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o kronecker.o kronecker.f90
!    gfortran -o kronecker.x kronecker.o
!    /usr/bin/time -f "%e %M %P" ./kronecker.x

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
PROGRAM kronecker
IMPLICIT NONE
    REAL, DIMENSION(:,:), allocatable :: Mat_A
    REAL, DIMENSION(:,:), allocatable :: Mat_B
    REAL, DIMENSION(:,:), allocatable :: Mat_C
    INTEGER :: A_1,A_2      ! Matriz A de tamaño A_1xA_2
    INTEGER :: B_1,B_2      ! Matriz B de tamaño B_1xB_2
    INTEGER :: C_1,C_2      ! Matriz C de tamaño C_1xC_2
    INTEGER :: alfa, beta
    INTEGER :: i, j, k, l
    A_1=2
    A_2=2
    B_1=2
    B_2=2
    C_1=A_1*B_1
    C_2=A_2*B_2
    allocate(Mat_A(A_1,A_2))               ! Se define el tamaño de la matriz A
    allocate(Mat_B(B_1,B_2))               ! Se define el tamaño de la matriz B
    allocate(Mat_C(C_1,C_2))               ! Se define el tamaño de la matriz C
    CALL random_number(Mat_A)
    CALL random_number(Mat_B)
    
    DO i=1, A_1
        DO j=1, A_2
            DO k=1, B_1
                DO l=1, B_2
                    alfa=B_1*(i-1)+k
                    beta=B_2*(j-1)+l
                    Mat_C(alfa,beta)=Mat_A(i,j)*Mat_B(k,l)
                END DO
            END DO
        END DO
    END DO 
    
    WRITE(*,*) Mat_A
    WRITE(*,*) Mat_B
    WRITE(*,*) Mat_C
END PROGRAM kronecker