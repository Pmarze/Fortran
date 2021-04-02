!    2021-04-01
!    MontyH.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que recrea las condiciones del problema de Monty Hall
!    para poder observar la probabilidad de éxito en caso de cambiar de
!    elección o no. El programa genera aleatoriamente la posición del 
!    participante y la posición del automóvil, se muestra la posición de
!    una de las cabras y con un 50% de probabilidad, se elige cambiar de 
!    puerta.

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o MontyH.o MontyH.f90
!    gfortran -o MontyH.x MontyH.o
!    ./MontyH.x

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

PROGRAM MontyH
IMPLICIT NONE
    INTEGER :: n=100000000                              ! Cantidad de iteraciones a realizar
    INTEGER :: posic, puert, decision, mostrar, resultado, i ! variables del problema
    INTEGER :: deccamb=0, decnocamb=0                   ! Decisión correcta era cambiar o no
    INTEGER, DIMENSION(3) :: puerta, cabras, puertaf    ! posición de participante, carro y cambio
    REAL :: a, b, c                                     ! variables para número aleatorio
    DO i=1, n                                           ! n iteraciones
        resultado=0                                     ! se debe reiniciar el valor de resultado
        CALL random_number(a)
        CALL random_number(b)
        CALL random_number(c)
        puert=INT(a*3)                                  ! Número aleatorio entero entre 0 y 2
        posic=INT(b*3)                                  ! Número aleatorio entero entre 0 y 2
        decision=INT(c*2)                               ! Número aleatorio entero entre 0 y 1
        CALL selpu(puert,puerta)                        ! Se asigna una puerta al participante
        !print*, 'posic', puerta
        CALL selca(posic,cabras)                        ! Se asigna una puerta al carro y cabras
        !print*, 'carro',cabras
        CALL mosca(puerta,cabras,mostrar)               ! Se elige que puerta mostrar al usuario
        CALL flip(puerta,mostrar,decision,puertaf)      ! Se cambia o no de puerta del usuario
        !print*, 'posic', puertaf
        CALL anali(puertaf,cabras,resultado)            ! Se analizan los resultados para saber si ganó
        !print*, 'resultado',resultado
        IF(decision==0)THEN                             ! Si decidió no cambiar
            IF (resultado==1)THEN                       ! La decision correcta era no cambiar
                !print*, 'no cambió y ganó'
                decnocamb=decnocamb+1
            ELSE
                !print*, 'no cambió y perdió'           ! La decisión correcta era cambiar
                deccamb=deccamb+1
            END IF
        ELSE
            IF(resultado==1)THEN                        ! La decisión correcta era cambiar
                !print*, 'cambió y ganó'
                deccamb=deccamb+1
            ELSE 
                !print*, 'cambió y perdió'              ! La decisión correcta era no cambiar
                decnocamb=decnocamb+1
            END IF
        END IF
    END DO                                              ! Se muestran los resultados
    print*, 'habia que cambiar=',deccamb,'no había que cambiar=',decnocamb
END PROGRAM MontyH

SUBROUTINE selpu(a,y)                           ! Función para asignar puerta al participante
    INTEGER, INTENT(IN) :: a                    ! Se ingresa un número aleatorio entre 0 y 2
    INTEGER, DIMENSION(3), INTENT(OUT) :: y     ! La salida es un vector de dimensión 3
    IF(a==0)THEN                                ! Se asigna al participante a la puerta 1
        y=(/1,0,0/)
    ELSE IF(a==1)THEN                           ! Se asigna al participante a la puerta 2
        y=(/0,1,0/)
    ELSE IF(a==2)THEN                           ! Se asigna al participante a la puerta 3
        y=(/0,0,1/)
    END IF
END SUBROUTINE selpu

SUBROUTINE selca(a,y)                           ! Función para asignar puerta al automóvil
    INTEGER, INTENT(IN) :: a                    ! Se ingresa un número aleatorio entre 0 y 2
    INTEGER, DIMENSION(3), INTENT(OUT) :: y     ! La salida es un vector de dimensión 3
    IF(a==0)THEN                                ! Se asigna el carro a la puerta 1 y las cabras al resto
        y=(/1,0,0/)
    ELSE IF(a==1)THEN                           ! Se asigna el carro a la puerta 1 y las cabras al resto
        y=(/0,1,0/)
    ELSE IF(a==2)THEN                           ! Se asigna el carro a la puerta 1 y las cabras al resto
        y=(/0,0,1/)
    END IF
END SUBROUTINE selca

SUBROUTINE mosca(a,b,y)                         ! Función para mostrar una de las cabras
                                                ! Las restricciones es no mostrar la puerta del automóvil 
                                                ! y tampoco la que el participante eligió
    INTEGER, DIMENSION(3), INTENT(IN) :: a, b   ! Se ingresa el vector posición del participante y del automóvil
    INTEGER, INTENT(OUT) :: y                   ! La salida es un número que indica que puerta se mostró
    IF(a(1)==1)THEN                             ! Si el participante está en a puerta 1
        IF(b(2)==0)THEN                         ! si hay una cabra en la puerta 2 se muestra esta puerta
            y=2
        ELSE                                    ! si no hay una cabra en 2, es porque hay un automóvil,
            y=3                                 ! entonces se muestra la puerta 3
        END IF
    ELSE IF(a(2)==1)THEN                        ! Si el participantes está en la puerta 2
        IF (b(1)==0)THEN                        ! Si hay una cabra en la puerta 1 se muestra esta puerta
            y=1
        ELSE                                    ! si no hay una cabra en 1, es porque hay un automóvil,
            y=3                                 ! entonces se muestra la puerta 3
        END IF
    ELSE IF(a(3)==1)THEN                        ! Si el participantes está en la puerta 3
        IF (b(1)==0)THEN                        ! Si hay una cabra en la puerta 1 se muestra esta puerta
            y=1
        ELSE                                    ! Si no hay una cabra en 1, es porque hay un automóvil
            y=2                                 ! entonces se muestra la puerta 2
        END IF
    END IF
END SUBROUTINE mosca

SUBROUTINE flip(a,b,c,y)                        ! Función para cambiar de puerta o no
    INTEGER, DIMENSION(3), INTENT(IN) :: a      ! Se ingresa la posición del participante
    INTEGER, INTENT(IN) :: b,c                  ! b nos dice donde se mostró la cabra, c=1 para cambiar de puerta
    INTEGER, DIMENSION(3), INTENT(OUT) :: y     ! Vector con la nueva posición del participante, si c=0 no hubo cambio
    y=a
    IF(c==1)THEN                                ! Si hay cambio, se invierten las posiciones dependiendo de la cabra mostrada
        IF(b==1)THEN                            ! Si la cabra mostrada estaba en 1, se cambia la posición de 2 a 3
            y(2)=a(3)
            y(3)=a(2)
        ELSE IF(b==2)THEN                       ! Si la cabra mostrada estaba en 2, se cambia la posición de 1 a 3
            y(1)=a(3)
            y(3)=a(1)
        ELSE IF(b==3)THEN                       ! Si la cabra mostrada estaba en 3, se cambia la posición de 1 a 2
            y(2)=a(1)
            y(1)=a(2)
        END IF
    ELSE                                        ! Si no hay cambio, todo queda igual
    END IF
END SUBROUTINE flip

SUBROUTINE anali(a,b,y)                         ! Función para analizar si ganó o no
    INTEGER, DIMENSION(3), INTENT(IN) :: a,b    ! se ingresa la posición del participante y la del automóvil
    INTEGER, INTENT(OUT) :: y                   ! Se dice si ganó o perdió
    INTEGER, DIMENSION(3) :: c                  ! Vector para sumar los vectores ingresados
    INTEGER :: i                                ! Variable de conteo
    c=a+b                                       ! Se suman ambos vectores, si el participante ganó, la suma de una coordenada
    DO i=1, 3                                   ! debe ser 2, en caso de perder, habrán dos cordenadas cuya suma sea 1
        IF (c(i)==2)THEN
            y=1                                 ! Si alguna de las coordenadas tiene un 2 significa que el participante ganó
        END IF
    END DO
END SUBROUTINE