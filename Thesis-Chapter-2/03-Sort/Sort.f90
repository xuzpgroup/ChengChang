PROGRAM main
IMPLICIT NONE

INTEGER :: i,j
REAL,ALLOCATABLE :: x(:),y(:),z(:),E(:)
INTEGER,ALLOCATABLE :: num(:)
REAL :: tmp_x,tmp_y,tmp_z,tmp_E,tmp_num
INTEGER :: n_pos
REAL :: E_Si,E_Li
REAL :: Eb_cut

OPEN(10,FILE='option')
  READ(10,*)
  READ(10,*) n_pos
  READ(10,*)
  READ(10,*) E_Si,E_Li
  READ(10,*)
  READ(10,*) Eb_cut
CLOSE(10)

ALLOCATE(x(n_pos))
ALLOCATE(y(n_pos))
ALLOCATE(z(n_pos))
ALLOCATE(E(n_pos))
ALLOCATE(num(n_pos))

OPEN(10,FILE='out-Eb-opt.dat')
  DO i=1,n_pos
    READ(10,*) x(i),y(i),z(i),E(i)
    num(i)=i
  END DO
CLOSE(10)

! Sort by energy E(i)
DO i=1,n_pos-1
  DO j=i+1,n_pos
    IF(E(j)<E(i)) THEN
      tmp_num=num(i)
      tmp_x=x(i)
      tmp_y=y(i)
      tmp_z=z(i)
      tmp_E=E(i)
      num(i)=num(j)
      x(i)=x(j)
      y(i)=y(j)
      z(i)=z(j)
      E(i)=E(j)
      num(j)=tmp_num
      x(j)=tmp_x
      y(j)=tmp_y
      z(j)=tmp_z
      E(j)=tmp_E
    END IF
  END DO
END DO

OPEN(10,FILE='out-Eb-opt-sort.dat')
  DO i=1,n_pos
    WRITE(10,"(F15.5,F15.5,F15.5,F15.5,I10)") x(i),y(i),z(i),E(i)-(E_Si+E_Li),num(i)
  END DO
CLOSE(10)

OPEN(10,FILE='out-Eb-opt-sort2.dat')
  DO i=1,n_pos
    tmp_E=E(i)-(E_Si+E_Li)
    IF(tmp_E < Eb_cut) THEN
      WRITE(10,"(F15.5,F15.5,F15.5,F15.5,I10)") x(i),y(i),z(i),E(i)-(E_Si+E_Li),num(i)
    END IF
  END DO
CLOSE(10)

END PROGRAM main
