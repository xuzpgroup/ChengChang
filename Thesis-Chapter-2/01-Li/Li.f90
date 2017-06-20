PROGRAM Li
IMPLICIT NONE

INTEGER :: i,j,k,m,n,p,q
INTEGER :: n_Si,n_Li
REAL :: xlo,xhi,ylo,yhi,zlo,zhi
INTEGER :: atom_type
REAL,ALLOCATABLE :: x_Si(:),y_Si(:),z_Si(:)
REAL :: dx,dy,dz
INTEGER :: nx,ny,nz
REAL :: x,y,z
REAL :: tmp_r,rc
INTEGER :: tmp_count
REAL :: tmp_x_Si,tmp_y_Si,tmp_z_Si

OPEN(10,FILE='option')
  READ(10,*) 
  READ(10,*) rc 
  READ(10,*) 
  READ(10,*) dx,dy,dz 
CLOSE(10)

WRITE(*,*) "Read a-Si.dat file"
OPEN(10,FILE='a-Si.dat')
  READ(10,*) n_Si
  ALLOCATE(x_Si(n_Si))
  ALLOCATE(y_Si(n_Si))
  ALLOCATE(z_Si(n_Si))
  READ(10,*)
  READ(10,*) xlo,xhi,ylo,yhi,zlo,zhi
  READ(10,*)
  DO i=1,n_Si
    READ(10,*) atom_type,x_Si(i),y_Si(i),z_Si(i)
  END DO
CLOSE(10)
WRITE(*,*) "-----END------"

nx=FLOOR((xhi-xlo)/dx)+1 
ny=FLOOR((yhi-ylo)/dy)+1
nz=FLOOR((zhi-zlo)/dz)+1

WRITE(*,*) "WRITE out-Li.dat file"
n_Li=0
OPEN(10,FILE='out-Li.dat')
DO i=1,nx
  DO j=1,ny
    DO k=1,nz
      x=xlo+(i-1)*dx
      y=ylo+(j-1)*dy
      z=zlo+(k-1)*dz
      tmp_count=0
      DO m=1,n_Si
        DO n=1,3
          DO p=1,3
            DO q=1,3
              tmp_x_Si=x_Si(m)+(n-2)*(xhi-xlo)
              tmp_y_Si=y_Si(m)+(p-2)*(yhi-ylo)
              tmp_z_Si=z_Si(m)+(q-2)*(zhi-zlo)
              tmp_r=SQRT((x-tmp_x_Si)**2+(y-tmp_y_Si)**2+(z-tmp_z_Si)**2)
              IF(tmp_r<rc) THEN
                tmp_count=1
              END IF
            END DO
          END DO
        END DO
      END DO
      IF(tmp_count==0) THEN
        WRITE(10,"(I5,F15.5,F15.5,F15.5)") 1,x,y,z
        n_Li=n_Li+1
      END IF
    END DO
  END DO
END DO
CLOSE(10)
WRITE(*,*) "-----END------"

WRITE(*,*) "WRITE out-Li.xyz file"
OPEN(10,FILE='out-Li.dat')
OPEN(20,FILE='out-Li.xyz')
  WRITE(20,*) n_Li
  WRITE(20,*) "Coordinate"
  DO i=1,n_Li
    READ(10,*) atom_type,x,y,z  
    WRITE(20,"(A5,F15.5,F15.5,F15.5)") "Li",x,y,z
  END DO
CLOSE(10)
CLOSE(20)
WRITE(*,*) "-----END------"

END PROGRAM Li
