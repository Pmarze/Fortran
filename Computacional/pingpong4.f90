!    mpifort -Wall -pedantic -std=f95 -c -o pingpong4.o pingpong4.f90
!    mpifort -o pingpong4.x pingpong4.o
!    mpirun -np 4 ./pingpong4.x |sort
PROGRAM pingpong4
USE mpi
    IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER :: ierr  
    INTEGER :: rank 
    INTEGER :: nprocs
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER,PARAMETER :: rebotes=2
    INTEGER :: pasoprima=1
    INTEGER :: enviado, recibido
    INTEGER :: pant, pact, ppos
    INTEGER :: i, nuc0, nuc1, nuc2, nuc3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    REAL :: r
    INTEGER :: a=0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    ! Inicializar MPI
    CALL MPI_Init(ierr)
    IF (ierr/=0) STOP 'MPI_Init error'

    ! Configurar el tamaño de comunicación
    CALL MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)
    IF (ierr/=0) STOP 'MPI_Comm_size error'
    IF (nprocs==4) THEN
        ! Configurar ranks/IDs para cada proceso
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
        IF (ierr/=0) STOP 'MPI_Comm_rank error'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
        Do i=1, rebotes
            pact=rank
            CALL random_number(r)
            a=r*4
            DO WHILE (a==pact) 
             CALL random_number(r)
             a=r*4
            END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
            IF (rank==0) THEN
                CALL MPI_SEND(0, 1, MPI_INT, 3, 0, MPI_COMM_WORLD, ierr)
                WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," GOLPE desde", rank, " hacia 3"   
                CALL MPI_RECV(recibido, 1, MPI_INT, 1, 1, MPI_COMM_WORLD, status, ierr)
                WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," RECIBO en", rank, " desde ", recibido
            ELSE IF (rank==1) THEN
                CALL MPI_SEND(1, 1, MPI_INT, 0, 1, MPI_COMM_WORLD, ierr)
                WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," GOLPE desde", rank, " hacia 0"   
                CALL MPI_RECV(recibido, 1, MPI_INT, 2, 2, MPI_COMM_WORLD, status, ierr)
                WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," RECIBO en", rank, " desde ", recibido            
            ELSE IF (rank==2) THEN
                CALL MPI_SEND(0, 1, MPI_INT, 1, 2, MPI_COMM_WORLD, ierr)
                WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," GOLPE desde", rank, " hacia 1"   
                CALL MPI_RECV(recibido, 1, MPI_INT, 3, 3, MPI_COMM_WORLD, status, ierr)
                WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," RECIBO en", rank, " desde ", recibido            
            ELSE IF (rank==3) THEN
                 CALL MPI_SEND(0, 1, MPI_INT, 2, 3, MPI_COMM_WORLD, ierr)
                WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," GOLPE desde", rank, " hacia 2"   
                CALL MPI_RECV(recibido, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, status, ierr)
                WRITE (*,'(A,I4,A,I2,A,I4)') "Salto ",i," RECIBO en", rank, " desde ", recibido                                                    
            END IF                                             
        END DO
        Call MPI_FINALIZE(ierr)
    END IF
END PROGRAM pingpong4