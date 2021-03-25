!    2020-03-14
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que encuentra los números de armstrong para distintas bases de b
!    y extrae los resultados en base decimal en un archivo de texto llamado
!    'Armstrong.txt', el programa se lleva a cabo en distintos núcleos


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    mpifort -Wall -pedantic -std=f95 -c -o ArmstrongParalelo.o ArmstrongParalelo.f90
!    mpifort -o ArmstrongParalelo.x ArmstrongParalelo.o
!    mpirun -np 8 ./ArmstrongParalelo.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./Armstrong.x

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

PROGRAM ArmstrongParalelo
USE MPI
IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER :: ierr                                 ! Variable para la señal de error
    INTEGER :: rank                                 ! ID del núcleo a usar
    INTEGER :: nprocs                               ! Número de procesadores a usar
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status   ! Estatus de envio o recibido
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    INTEGER :: npb=1000000000                       ! Cantidad de números por base
    !INTEGER :: bmin=1, bama=17                      ! Base mínima, Base máxima
    INTEGER :: bmin=1, bama=7                      ! Base mínima, Base máxima
    !INTEGER :: bmin=1, bama=3                      ! Base mínima, Base máxima
    INTEGER :: b, x, k, d, f , i, bcal, rond, paso  ! Variables del problema
    REAL :: xreal,breal                             ! Necesario para la función que calcula "k"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER, DIMENSION(8) :: wvec=(/0,0,0,0,0,0,0,0/)
    INTEGER :: a
    CALL MPI_Init(ierr)                             ! Inicializar MPI
    IF (ierr/=0) STOP 'MPI_Init error'              ! En caso de error se detiene el proceso
    CALL MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)  ! Configura el tamaño de comunicación
    IF (ierr/=0) STOP 'MPI_Comm_size error'         ! En caso de error se detiene el proceso
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)    ! Configura el ID de cada proc.
    IF (ierr/=0) STOP 'MPI_Comm_rank error'         ! En caso de error se detiene el proceso
    bcal=bama-bmin
    IF (bcal<=0) STOP 'Orden incorrecto de cálculo' ! Solo interesa calculo de basses ascendentes
    IF (nprocs>=9) STOP 'Más procesadores de los programados'   ! Programa para 8 o menos núcleos
    
    IF (nprocs==(bcal-1)) THEN
        DO a=1, nprocs
            wvec(a)=1
        END DO
    ELSE IF (nprocs>bcal) THEN
        DO a=1, bcal+1
            wvec(a)=1
        END DO
    ELSE IF(nprocs<=bcal)THEN
        rond=FLOOR(REAL(bcal)/REAL(nprocs))
        DO a=1, nprocs
            wvec(a)=1*rond
        END DO
        paso=MOD(bcal,nprocs)+1
        DO a=1, paso
            wvec(a)=wvec(a)+1
        END DO
    END IF
    print*, wvec(1),wvec(2),wvec(3),wvec(4),wvec(5),wvec(6),wvec(7),wvec(8)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    Call MPI_FINALIZE(ierr)         ! Finaliza MPI    
END PROGRAM ArmstrongParalelo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

SUBROUTINE di(i,n,b,y)                      ! Subrutina para encontrar el coeficiente d_i
    INTEGER, INTENT(IN) :: i, n, b
    INTEGER, INTENT(OUT) :: y
    y=(MOD(n,b**(i+1))-MOD(n,b**i))/(b**i)  ! Fórmula dada para calcular
END SUBROUTINE di

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

SUBROUTINE kb(x,b,y)                        ! Subrutina para encontrar el coeficiente k
    REAL, INTENT(IN) :: b,x
    INTEGER, INTENT(OUT) :: y
    REAL :: z
    z=LOG(x)/LOG(b)                         ! Formula para el cambio de logaritmo a base b
    y=INT(z)+1                              ! El resultado es un entero y se devuelve como tal
END SUBROUTINE kb

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    