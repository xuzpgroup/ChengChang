PROGRAM Select_Li
IMPLICIT NONE

INTEGER :: i,j,k,m,n
INTEGER :: n_pos
REAL,ALLOCATABLE :: x(:),y(:),z(:),E(:),retain(:)
INTEGER,ALLOCATABLE :: num(:)
REAL :: tmp_r,rc
REAL :: xlo,xhi,ylo,yhi,zlo,zhi
INTEGER :: tmp_count

OPEN(10,FILE='option')
  READ(10,*)
  READ(10,*) rc
  READ(10,*)
  READ(10,*) n_pos
CLOSE(10)

ALLOCATE(x(n_pos))
ALLOCATE(y(n_pos))
ALLOCATE(z(n_pos))
ALLOCATE(E(n_pos))
ALLOCATE(num(n_pos))
ALLOCATE(retain(n_pos))

OPEN(10,FILE='out-Eb-opt-sort2.dat')
  DO i=1,n_pos 
    READ(10,*) x(i),y(i),z(i),E(i),num(i)
  END DO
CLOSE(10)

OPEN(10,FILE='a-Si.dat')
READ(10,*)
READ(10,*)
READ(10,*) xlo,xhi,ylo,yhi,zlo,zhi
CLOSE(10)


! The ith Li atom retain 1 or remove 0
retain(:)=1

DO i=2,n_pos 
  DO j=1,i-1
    IF(retain(j)==1) THEN
      tmp_count=0
      DO k=1,3
        DO m=1,3
          DO n=1,3
            tmp_r=SQRT((x(i)-x(j)+(k-2)*(xhi-xlo))**2+(y(i)-y(j)+(m-2)*(yhi-ylo))**2+(z(i)-z(j)+(n-2)*(zhi-zlo))**2)
            IF(tmp_r<rc) THEN
              tmp_count=1
            END IF
          END DO
        END DO
      END DO 
      IF(tmp_count==1) THEN
        IF(E(i)>E(j)) THEN
          retain(i)=0
        ELSE
          retain(j)=0
        END IF
      END IF
    END IF
  END DO
END DO

OPEN(10,FILE='out-Eb-opt-select.dat')
OPEN(20,FILE='out-Eb-opt-select.xyz')
  WRITE(20,*) 
  DO i=1,n_pos
    IF(retain(i)==1) THEN
      WRITE(10,"(F15.5,F15.5,F15.5,F15.5,I10)") x(i),y(i),z(i),E(i),num(i)
      WRITE(20,"(A5,F15.5,F15.5,F15.5,I10)") "Li",x(i),y(i),z(i),num(i)
    END IF
  END DO
CLOSE(10)
CLOSE(20)

END PROGRAM Select_Li
