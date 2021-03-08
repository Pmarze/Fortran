!    2020-03-07
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que juega ping pong entre dos núcleos parelelos,
!    Se interpretó las dos dimensiones como un vector de 3 coordenadas
!    En el cual la "altura" se dice que es la posición 1,2 o 3.
!    Durante cada ciclo se envía un número aleatorio entre 0-50 el cual
!    se suma al valor anterior de la coordenada. Al finalizar se muestra el
!    valor de cada vector i, así como el núcleo al que pertenece.

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    mpifort -Wall -pedantic -std=f95 -c -o pingpong2d.o pingpong2d.f90
!    mpifort -o pingpong2d.x pingpong2d.o
!    mpirun -np 2 ./pingpong2d.x |sort

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
PROGRAM pingpong2d
USE MPI 
IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER :: ierr                                 ! Variable para la señal de error
INTEGER :: rank                                 ! ID del núcleo a usar
INTEGER :: nprocs                               ! Número de procesadores a usar
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status   ! Estatus de envio o recibido
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER,PARAMETER :: rebotes=15         ! Cantidad de rebotes a realizar
INTEGER :: enviado=0, recibido=0        ! Datos que se envian o reciben
INTEGER :: a                            ! Variable para contar los ciclos a repetir
REAL :: r                               ! Variable para generar el número aleatorio
INTEGER, DIMENSION(3) :: i=(/0,0,0/)    ! Vector con posición abajo, enmedio, arriba
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL MPI_Init(ierr)                             ! Inicializar MPI
IF (ierr/=0) STOP 'MPI_Init error'              ! En caso de error se detiene el proceso
CALL MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)  ! Configura el tamaño de comunicación
IF (ierr/=0) STOP 'MPI_Comm_size error'         ! En caso de error se detiene el proceso
IF (nprocs==2) THEN                             ! El programa funciona solo si son 4 proc.
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)! Configura el ID de cada proc.
    IF (ierr/=0) STOP 'MPI_Comm_rank error'     ! En caso de error se detiene el proceso
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO a=0, rebotes             ! El ciclo se repite "rebotes" veces
    CALL random_number(r)   ! Se genera un número al azar
    enviado=r*50            ! Será un número entre 0-50, este se enviará a posición del vector
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IF (rank==0) THEN           ! Si estamos en el núcleo 0 
        ! Se envia un número aleatorio al procesador 1
        CALL MPI_SEND(enviado, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, ierr)
            WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",a," GOLPE desde", rank, " hacia ", 1
        ! Se recibe un dato desde el procesador 1 y se almacena en la coordenada MOD(a,3)+1
        CALL MPI_RECV(recibido, 1, MPI_INT, 1, 1, MPI_COMM_WORLD, status, ierr)
        WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",a," RECIBO en", rank, " desde", 1
        i(MOD(a,3)+1)=i(MOD(a,3)+1)+recibido
    ELSE IF (rank==1) THEN      ! Si estamos en el núcleo 1
        ! Se envia un número aleatorio al procesador 0
        CALL MPI_SEND(enviado, 1, MPI_INT, 0, 1, MPI_COMM_WORLD, ierr)
        WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",a," GOLPE desde", rank, " hacia ", 0
        ! Se recibe un dato desde el procesador 0 y se almacena en la coordenada MOD(a,3)+1
        CALL MPI_RECV(recibido, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, status, ierr)
        WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",a," RECIBO en", rank, " desde", 0
        i(MOD(a,3)+1)=i(MOD(a,3)+1)+recibido
    END IF                                             
END DO
WRITE(*,*) 'i(1)=', i(1),'i(2)=', i(2),'i(3)=', i(3), 'nucleo#', rank
Call MPI_FINALIZE(ierr)         ! Finaliza MPI
END IF
END PROGRAM pingpong2d    