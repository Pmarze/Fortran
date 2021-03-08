!    2020-03-03
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que juega ping pong entre cuetro núcleos parelelos,
!    la dirección de envío de los golpes se varia en 3 formas distintas
!    para observar de mejor forma el funcionamiento de MPI

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    mpifort -Wall -pedantic -std=f95 -c -o pingpong4.o pingpong4.f90
!    mpifort -o pingpong4.x pingpong4.o
!    mpirun -np 4 ./pingpong4.x |sort

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

PROGRAM pingpong4
USE mpi
IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER :: ierr                                 ! Variable para la señal de error
INTEGER :: rank                                 ! ID del núcleo a usar
INTEGER :: nprocs                               ! Número de procesadores a usar
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status   ! Estatus de envio o recibido
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER,PARAMETER :: rebotes=15         ! Cantidad de rebotes a realizar
INTEGER :: enviado=0, recibido=1        ! Datos que se envian o reciben
INTEGER,DIMENSION(8) :: nuc=(/3,0,1,2,1,2,3,0/)
INTEGER :: i                            ! i para realizar el conteo
INTEGER :: a=0                          ! a para seleccionar la permutación de envío
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL MPI_Init(ierr)                             ! Inicializar MPI
IF (ierr/=0) STOP 'MPI_Init error'              ! En caso de error se detiene el proceso
CALL MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)  ! Configura el tamaño de comunicación
IF (ierr/=0) STOP 'MPI_Comm_size error'         ! En caso de error se detiene el proceso
IF (nprocs==4) THEN                             ! El programa funciona solo si son 4 proc.
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)! Configura el ID de cada proc.
    IF (ierr/=0) STOP 'MPI_Comm_rank error'     ! En caso de error se detiene el proceso
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    Do i=1, rebotes                 ! El programa se repite la cantidad solicitada
        ! Dependiendo del valor de a, se elige una permutación de envío, para el caso de
        ! a=0 tenemos que el nucleo 0 envía a 3, 1 envía a 0, 2 envía a 1 y 3 envía a 2,
        ! esto es de forma didáctica para variar los canales de comunicación entre núcleos
        IF(a==0)THEN                ! Permutación 0->3, 1->0, 2->1, 3->2
            nuc=(/3,0,1,2,1,2,3,0/)
        ELSE IF(a==1) THEN          ! Permutación 0->2, 1->3, 2->0, 3->1
            nuc=(/2,3,0,1,2,3,0,1/)
        ELSE IF(a==2) THEN          ! Permutación 0->1, 1->2, 2->3, 3->0
            nuc=(/1,2,3,0,3,0,1,2/)
        END IF
        a=MOD(a+1,3)                ! Solo se tienen 3 variaciones, si a=4 se vuelve a 0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (rank==0) THEN           ! Si estamos en el núcleo 0 
            ! Se envia un dato al procesador dado por nuc0e
            CALL MPI_SEND(enviado, 1, MPI_INT, nuc(1), 0, MPI_COMM_WORLD, ierr)
                WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," GOLPE desde", rank, " hacia ", nuc(1)   
            ! Se recibe un dato desde el procesador dado por nuc0r
            CALL MPI_RECV(recibido, 1, MPI_INT, nuc(5), nuc(5), MPI_COMM_WORLD, status, ierr)
            WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," RECIBO en", rank, " desde", nuc(5)
        ELSE IF (rank==1) THEN      ! Si estamos en el núcleo 1
            ! Se envia un dato al procesador dado por nuc1e
            CALL MPI_SEND(enviado, 1, MPI_INT, nuc(2), 1, MPI_COMM_WORLD, ierr)
            WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," GOLPE desde", rank, " hacia ", nuc(2)   
            ! Se recibe un dato desde el procesador dado por nuc1r
            CALL MPI_RECV(recibido, 1, MPI_INT, nuc(6), nuc(6), MPI_COMM_WORLD, status, ierr)
            WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," RECIBO en", rank, " desde", nuc(6)
        ELSE IF (rank==2) THEN      ! Si estamos en el núcleo 2
            ! Se envia un dato al procesador dado por nuc2e
            CALL MPI_SEND(enviado, 1, MPI_INT, nuc(3), 2, MPI_COMM_WORLD, ierr)
            WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," GOLPE desde", rank, " hacia ", nuc(3)   
            ! Se recibe un dato desde el procesador dado por nuc2r
            CALL MPI_RECV(recibido, 1, MPI_INT, nuc(7), nuc(7), MPI_COMM_WORLD, status, ierr)
            WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," RECIBO en", rank, " desde", nuc(7)
        ELSE IF (rank==3) THEN      ! Si estamos en el núcleo 3
            ! Se envia un dato al procesador dado por nuc3e 
            CALL MPI_SEND(enviado, 1, MPI_INT, nuc(4), 3, MPI_COMM_WORLD, ierr)
            WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," GOLPE desde", rank, " hacia ", nuc(4)      
            ! Se recibe un dato desde el procesador dado por nuc3r
            CALL MPI_RECV(recibido, 1, MPI_INT, nuc(8), nuc(8), MPI_COMM_WORLD, status, ierr)
            WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," RECIBO en", rank, " desde", nuc(8)
        END IF                                             
    END DO
    Call MPI_FINALIZE(ierr)         ! Finaliza MPI
END IF
END PROGRAM pingpong4