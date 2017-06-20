PROGRAM main
IMPLICIT NONE

INTEGER :: i,nLi,n,connect
REAL :: Eb_min,Eb_max,Eij,Eji

OPEN(10,FILE='option')
READ(10,*)
READ(10,*) n,Eb_min,Eb_max
CLOSE(10)


OPEN(10,FILE='connect4.dat')
OPEN(20,FILE='connect5.dat')
DO i=1,n
  READ(10,*) connect,Eij,Eji
  IF(Eij > Eb_min .AND. Eij < Eb_max .AND. Eji > Eb_min .AND. Eji < Eb_max) THEN
    WRITE(20,"(I10,F15.5,F15.5)") connect,Eij,Eji 
  ELSE
    connect=0
    WRITE(20,"(I10,F15.5,F15.5)") connect,Eij,Eji 
  END IF
END DO
CLOSE(10)
CLOSE(20)

END PROGRAM main
