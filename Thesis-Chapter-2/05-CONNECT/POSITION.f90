PROGRAM main
IMPLICIT NONE

INTEGER :: i
INTEGER :: n_pos
REAL :: x,y,z,E

OPEN(10,FILE='structures.dat')
OPEN(20,FILE='position.dat')
  READ(10,*)
  READ(10,*) n_pos
  READ(10,*)
  READ(10,*)
  READ(10,*)
  READ(10,*)
  READ(10,*)
  WRITE(20,*) "# n_pos"
  WRITE(20,*) n_pos
  WRITE(20,*) "# n_particle"
  WRITE(20,*)
  WRITE(20,*) "# position, particle, energy"
  DO i=1,n_pos
    READ(10,*) x,y,z,E
    WRITE(20,*) i,"   0   ",E
  END DO
CLOSE(10)
CLOSE(20)

END PROGRAM main
