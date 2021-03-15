!    2020-03-14
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que 


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    mpifort -Wall -pedantic -std=f95 -c -o Armstrong.o Armstrong.f90
!    mpifort -o Armstrong.x Armstrong.o
!    mpirun -np 2 ./Armstrong.x

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
PROGRAM Armstrong
    IMPLICIT NONE
    INTEGER :: npb, bam              ! Numeros por base, base máxima
    INTEGER :: b, x, k, d, f         ! Variables del problema
    DO b=0, bam
        DO x=0, npb
            CALL kb(x,b,k)
            CALL Fb(b,x,k,f)
            IF (f==x)THEN
                PRINT *, f,'es un número de armstrong en la base',b
            END IF
        END DO
    END DO
END PROGRAM Armstrong

SUBROUTINE Fb(b,n,k,y)
    INTEGER, INTENT(IN) :: b, n, k
    INTEGER, INTENT(OUT) :: y
    INTEGER :: a1, a2, a3
    DO a1=0, k-1
        di(n,b,a2)
        a3=a3+a2**k
    END DO
    y=a3
END SUBROUTINE

SUBROUTINE di(i,n,b,y)
    INTEGER, INTENT(IN) :: i, n, b
    INTEGER, INTENT(OUT) :: y
    y=(MOD(n,b**(i+1))-MOD(n,b**i))/(b**i)
END SUBROUTINE

SUBROUTINE kb(x,b,y)
    REAL, INTENT(IN) :: b,x
    INTEGER, INTENT(OUT) :: y 
    REAL :: z
    z=LOG(x)/LOG(b)
    y=INT(z)+1
END SUBROUTINE