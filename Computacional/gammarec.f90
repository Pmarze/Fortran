!    2020-03-14
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que por medio de recursividad calcula los valores de la función
!    Gamma para valores enteros no negativos (Naturales).


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o gammarec.o gammarec.f90
!    gfortran -o gammarec.x gammarec.o
!    /usr/bin/time -f "%e %M %P" ./gammarec.x

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

PROGRAM gammarec
IMPLICIT NONE
    REAL(8) :: z, s
    REAL(8) :: gama
    z=7
    s=gama(z)
    PRINT*,s

END PROGRAM gammarec

RECURSIVE FUNCTION gama(a) RESULT(f)
    REAL(8), INTENT(IN) :: a
    REAL(8) :: f
    IF (a<=1)THEN
        f=1
    ELSE
        f=(a-1)*gama(a-1)
    END IF
END FUNCTION