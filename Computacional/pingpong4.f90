PROGRAM pingpong4
!    mpifort -Wall -pedantic -std=f95 -c -o pingpong4.o pingpong4.f90
!    mpifort -o pingpong4.x pingpong4.o
!    mpirun -np 1 ./pingpong4.x
    USE mpi
    IMPLICIT NONE

    INTEGER :: error    ! Variable para la señal de error
    INTEGER :: nprocs   ! Cantidad n de procesos
    INTEGER :: idnucleo ! Process ID (pid)
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status

    INTEGER,PARAMETER :: rebotes=10
    INTEGER :: dato
    REAL :: posicion, panterior
    INTEGER :: paleta
    INTEGER :: nuc0, nuc1, nuc2, nuc3
    INTEGER :: i

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    REAL :: r
    INTEGER :: a
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    ! Inicializar MPI
    CALL MPI_Init(error)
    IF (error/=0) STOP 'MPI_Init error'

    ! Configurar el tamaño de comunicación
    CALL MPI_Comm_size(MPI_COMM_WORLD,nprocs,error)
    IF (error/=0) STOP 'MPI_Comm_size error'

    ! Configurar ranks/IDs para cada proceso
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,idnucleo,error)
    IF (error/=0) STOP 'MPI_Comm_rank error'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    posicion=0
    nuc0=0
    nuc1=0
    nuc2=0
    nuc3=0

    Do i=1, rebotes
        Call random_number(r)
        a=r*4
        !WRITE (*,*) a,a
        DO WHILE (a==posicion) 
            Call random_number(r)
            a=r*4
        END DO
        panterior=posicion
        posicion=a
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
        
        IF (a==0) THEN
            WRITE(*,*) 'nuc0'
        ELSE IF (a==1) THEN
            WRITE(*,*) 'nuc1'
        ELSE IF (a==2) THEN
            WRITE(*,*) 'nuc2'
        ELSE IF (a==3) THEN
            WRITE(*,*) 'nuc3'
        END IF
    END DO
END PROGRAM pingpong4