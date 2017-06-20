PROGRAM CONNECT
IMPLICIT NONE

INTEGER :: i,j,k,m,n
INTEGER :: n_pos
REAL,ALLOCATABLE :: x(:),y(:),z(:)
REAL :: rc,tmp_r
REAL :: xlo,xhi,ylo,yhi,zlo,zhi
REAL :: lx,ly,lz
INTEGER :: c_ij

OPEN(10,FILE='structures.dat')
  READ(10,*) 
  READ(10,*) n_pos
  ALLOCATE(x(n_pos))
  ALLOCATE(y(n_pos))
  ALLOCATE(z(n_pos))
  READ(10,*) 
  READ(10,*) rc 
  READ(10,*) 
  READ(10,*) xlo,xhi,ylo,yhi,zlo,zhi
  lx=xhi-xlo
  ly=yhi-ylo
  lz=zhi-zlo
  READ(10,*) 
  DO i=1,n_pos
    READ(10,*) x(i),y(i),z(i) 
  END DO
CLOSE(10)

OPEN(10,FILE='connect.dat')
OPEN(20,FILE='connect2.dat')
OPEN(30,FILE='connect3.dat')
OPEN(32,FILE='connect32.dat')
DO i=1,n_pos
  DO j=i+1,n_pos
    c_ij=0
    DO k=1,3
      DO m=1,3
        DO n=1,3
          tmp_r=SQRT((x(i)-x(j)-(k-2)*lx)**2+(y(i)-y(j)-(m-2)*ly)**2+(z(i)-z(j)-(n-2)*lz)**2)
          IF(tmp_r <= rc) THEN
            c_ij=1
          END IF
        END DO
      END DO
    END DO
    WRITE(10,*) c_ij
    WRITE(32,*) i,j,c_ij
    IF (c_ij==1) THEN
      WRITE(20,"(F15.5,F15.5,F15.5,F15.5,F15.5,F15.5)") x(i),y(i),z(i),x(j),y(j),z(j)
      WRITE(30,*) i,j
    END IF
  END DO 
END DO
CLOSE(10)
CLOSE(20)
CLOSE(30)
CLOSE(32)

END PROGRAM CONNECT
