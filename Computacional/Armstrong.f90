!    2020-03-14
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que 


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o Armstrong.o Armstrong.f90
!    gfortran -o Armstrong.x Armstrong.o
!    ./Armstrong.x

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
    INTEGER :: npb=100000000, bmin=2, bam=10              ! Numeros por base, base máxima
    INTEGER :: b, x, k, d, f1,f2 , i              ! Variables del problema
    REAL :: xreal,breal
    OPEN(12,file='Armstrong.txt')
    print*, 'números por base=',npb
    DO b=bmin, bam
        print*, 'base=',b
        WRITE(12,*) 'Base=',b
        DO x=0, npb
            xreal=REAL(x)
            breal=REAL(b)
            CALL kb(xreal,breal,k)
!            IF (k>1)THEN
                DO i=0, k-1
                    CALL di(i,x,b,d)
                    f1=f2
                    f2=d**k+f1
                END DO
            !END IF
            IF (f2==x)THEN
                print*,f2,'es un número de Armstrong'
                WRITE(12,*) f2
            END IF
            f2=0
        END DO
    END DO
END PROGRAM Armstrong

SUBROUTINE di(i,n,b,y)
    INTEGER, INTENT(IN) :: i, n, b
    INTEGER, INTENT(OUT) :: y
    y=(MOD(n,b**(i+1))-MOD(n,b**i))/(b**i)
END SUBROUTINE di

SUBROUTINE kb(x,b,y)
    REAL, INTENT(IN) :: b,x
    INTEGER, INTENT(OUT) :: y
    REAL :: z
    z=LOG(x)/LOG(b)
    y=INT(z)+1
END SUBROUTINE kb