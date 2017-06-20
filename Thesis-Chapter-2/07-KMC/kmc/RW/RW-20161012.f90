! Modified on 20161012

PROGRAM main
IMPLICIT NONE

INTEGER :: i,j,k,m,mm
INTEGER :: n_pos,n_particle,n_step
INTEGER,ALLOCATABLE :: connect(:,:)
REAL,ALLOCATABLE :: Eb(:,:)
INTEGER :: tmp_pos,tmp_particle
INTEGER,ALLOCATABLE :: pos(:),pos_0(:)
REAL,ALLOCATABLE :: dE(:)
REAL :: t,dt
REAL :: dG,dG0,alpha,kB,Temp
REAL :: v,v1
REAL,ALLOCATABLE :: p_ik(:)
REAL :: tmp_p,u1,u2
INTEGER :: hop_particle,hop_pos
INTEGER,ALLOCATABLE :: nx(:),ny(:),nz(:)
REAL :: xlo,xhi,ylo,yhi,zlo,zhi,lx,ly,lz
REAL,ALLOCATABLE :: x(:),y(:),z(:),xu(:),yu(:),zu(:),tmp_x(:),tmp_y(:),tmp_z(:),x0(:),y0(:),z0(:)
REAL,ALLOCATABLE :: x_0(:),y_0(:),z_0(:)
REAL,ALLOCATABLE :: str_x(:),str_y(:),str_z(:)
REAL,ALLOCATABLE :: MSD(:),Tot_MSD(:)
REAL :: tmp_MSD,tmp_Tot_MSD
INTEGER :: n_out
REAL :: tmp_t 


WRITE(*,*) "01-Read option"
OPEN(10,FILE='option')
  READ(10,*) 
  READ(10,*) n_step 
  READ(10,*)
  READ(10,*) v,kB,Temp,dG0,alpha
  v1=1/v
  ! WRITE(*,*) v,v1
  READ(10,*)
  READ(10,*) n_out
CLOSE(10)

! Particles distribution, energy in pos i
! Particle occupy the pos i, pos(i)=k means particle k occupy pos i, pos(i)=0
! means no particle in pos i
WRITE(*,*) "02-Read position.dat"
OPEN(10,FILE='position.dat')
  READ(10,*)
  READ(10,*) n_pos
  READ(10,*)
  READ(10,*) n_particle
  READ(10,*) 
  ALLOCATE(pos(n_pos))
  ALLOCATE(dE(n_pos))
  DO i=1,n_pos
    READ(10,*) tmp_pos,pos(i),dE(i)
  END DO
CLOSE(10)
WRITE(*,*) "Read position-0.dat"
OPEN(10,FILE='position-0.dat')
  READ(10,*)
  READ(10,*)
  READ(10,*)
  READ(10,*)
  READ(10,*) 
  ALLOCATE(pos_0(n_pos))
  DO i=1,n_pos
    READ(10,*) tmp_pos,pos_0(i)
  END DO
CLOSE(10)

WRITE(*,*) "03-Read structures.dat"
ALLOCATE(str_x(n_pos))
ALLOCATE(str_y(n_pos))
ALLOCATE(str_z(n_pos))
OPEN(10,FILE='structures.dat')
  DO i=1,5
    READ(10,*)
  END DO
  READ(10,*) xlo,xhi,ylo,yhi,zlo,zhi
  lx=xhi-xlo
  ly=yhi-ylo
  lz=zhi-zlo
  READ(10,*)
  DO i=1,n_pos
    READ(10,*) str_x(i),str_y(i),str_z(i)
  END DO
CLOSE(10)

! Connectivity between pos i and j, =1 connect
      !--20161012--
! WRITE(*,*) "04-Read connect.dat"
WRITE(*,*) "04-Read connect5.dat"
      !--20161012--
! WRITE(*,*) "05-Write tmp-connect.dat"
ALLOCATE(connect(n_pos,n_pos))
      !--20161012--
ALLOCATE(Eb(n_pos,n_pos))
      !--20161012--
! OPEN(10,FILE='connect.dat')
OPEN(10,FILE='connect5.dat')
      !--20161012--
! OPEN(20,FILE='tmp-connect.dat')
  DO i=1,n_pos
    DO j=i+1,n_pos
      !--20161012--
      ! READ(10,*) connect(i,j)
      READ(10,*) connect(i,j),Eb(i,j),Eb(j,i)
      !--20161012--
      connect(j,i)=connect(i,j)
    END DO
    DO j=1,n_pos
      IF(j==i) THEN
        connect(i,j)=2
      END IF
    END DO
    ! WRITE(20,*) connect(i,:)
  END DO
CLOSE(10)
! CLOSE(20)

! Kinetic Monte Carlo - KMC
CALL init_random_seed()
ALLOCATE(p_ik(n_particle*n_pos))


! Initial positions of particles
ALLOCATE(x(n_particle))
ALLOCATE(y(n_particle))
ALLOCATE(z(n_particle))
ALLOCATE(x0(n_particle))
ALLOCATE(y0(n_particle))
ALLOCATE(z0(n_particle))
ALLOCATE(xu(n_particle))
ALLOCATE(yu(n_particle))
ALLOCATE(zu(n_particle))
ALLOCATE(tmp_x(n_particle))
ALLOCATE(tmp_y(n_particle))
ALLOCATE(tmp_z(n_particle))
ALLOCATE(nx(n_particle))
ALLOCATE(ny(n_particle))
ALLOCATE(nz(n_particle))
! DO j=1,n_particle
!   nx(j)=0
!   ny(j)=0
!   nz(j)=0
! END DO
WRITE(*,*) "READ Periocid-box.dat"
OPEN(10,FILE='Periodic-box.dat')
  READ(10,*)
  READ(10,*) tmp_t
  t=tmp_t*v
  READ(10,*)
  DO i=1,n_particle
    READ(10,*) tmp_particle,nx(i),ny(i),nz(i) 
  END DO
CLOSE(10)

ALLOCATE(x_0(n_particle))
ALLOCATE(y_0(n_particle))
ALLOCATE(z_0(n_particle))
DO j=1,n_particle
  DO i=1,n_pos
    IF(pos_0(i)==j) THEN
      x_0(j)=str_x(i)
      y_0(j)=str_y(i)
      z_0(j)=str_z(i)
    END IF
  END DO
END DO

DO j=1,n_particle
  DO i=1,n_pos
    IF(pos(i)==j) THEN
      x(j)=str_x(i)
      y(j)=str_y(i)
      z(j)=str_z(i)
      x0(j)=x(j)
      y0(j)=y(j)
      z0(j)=z(j)
      tmp_x(j)=x(j)
      tmp_y(j)=y(j)
      tmp_z(j)=z(j)
    END IF
  END DO
END DO


ALLOCATE(MSD(n_particle))
ALLOCATE(Tot_MSD(n_particle))
WRITE(*,*) "06-WRITE out-xyz.dat"
WRITE(*,*) "07-WRITE out-xuyuzu.dat"
WRITE(*,*) "08-WRITE out-MSD.dat"
OPEN(30,FILE='out-xyz.dat')
OPEN(40,FILE='out-xuyuzu.dat')
OPEN(50,FILE='out-MSD.dat')
OPEN(60,FILE='out-Tot-MSD.dat')
OPEN(70,FILE='out-pos.dat')
! t=0.0
WRITE(30,*) n_particle
! WRITE(30,*) "Step",m,"Time(s)",t/v 
WRITE(30,*) "Step",m,"Time(s)",t*v1
WRITE(40,*) n_particle
! WRITE(40,*) "Step",m,"Time(s)",t/v 
WRITE(40,*) "Step",m,"Time(s)",t*v1
WRITE(70,*) n_particle
WRITE(70,*) "Step",m,"Time(s)",t*v1
DO j=1,n_particle
  WRITE(30,*) j,x(j),y(j),z(j)
  WRITE(40,*) j,xu(j),yu(j),zu(j)
  DO k=1,n_pos
    IF(pos(k)==j) THEN
      WRITE(70,*) j,k,dE(k)
    END IF
  END DO
END DO
! WRITE(50,*) "Time(s), MSD(m^2)"
WRITE(50,*) "Time(s), MSD(m^2)"
WRITE(50,*) 0.0,0.0
WRITE(60,*) "Time(s), MSD(m^2)"
WRITE(60,*) 0.0,0.0
dt=0.0
DO m=1,n_step

  DO j=1,n_particle*n_pos
    p_ik(j)=0.0
  END DO

  ! Write out-xyz.dat
  ! Write out-xuyuzu.dat
  ! WRITE(30,*) n_particle
  ! WRITE(30,*) "Step",m,"Time(s)",t/v 
  ! WRITE(40,*) n_particle
  ! WRITE(40,*) "Step",m,"Time(s)",t/v 
  DO j=1,n_particle
    DO i=1,n_pos
      IF(pos(i)==j) THEN
        x(j)=str_x(i)
        y(j)=str_y(i)
        z(j)=str_z(i)
        ! WRITE(30,*) j,x(j),y(j),z(j)
        IF(x(j)-tmp_x(j) > lx/2) THEN
          nx(j)=nx(j)-1
        END IF
        IF(tmp_x(j)-x(j) > lx/2) THEN
          nx(j)=nx(j)+1
        END IF
        IF(y(j)-tmp_y(j) > ly/2) THEN
          ny(j)=ny(j)-1
        END IF
        IF(tmp_y(j)-y(j) > ly/2) THEN
          ny(j)=ny(j)+1
        END IF
        IF(z(j)-tmp_z(j) > lz/2) THEN
          nz(j)=nz(j)-1
        END IF
        IF(tmp_z(j)-z(j) > lz/2) THEN
          nz(j)=nz(j)+1
        END IF
        xu(j)=x(j)+nx(j)*lx
        yu(j)=y(j)+ny(j)*ly
        zu(j)=z(j)+nz(j)*lz
        ! WRITE(40,*) j,xu(j),yu(j),zu(j)
        MSD(j)=(xu(j)-x0(j))**2+(yu(j)-y0(j))**2+(zu(j)-z0(j))**2
        Tot_MSD(j)=(xu(j)-x_0(j))**2+(yu(j)-y_0(j))**2+(zu(j)-z_0(j))**2
        tmp_x(j)=x(j)
        tmp_y(j)=y(j)
        tmp_z(j)=z(j)
      END IF
    END DO
  END DO
  tmp_MSD=0.0
  tmp_Tot_MSD=0.0
  DO j=1,n_particle
    tmp_MSD=tmp_MSD+MSD(j)
    tmp_Tot_MSD=tmp_Tot_MSD+Tot_MSD(j) 
  END DO
  ! WRITE(50,*) t/v,tmp_MSD*1e-20/n_particle
  ! WRITE(50,*) t/v,tmp_MSD/n_particle
  DO mm=1,n_step/n_out
    IF (m==mm*n_out) THEN
      WRITE(30,*) n_particle
      ! WRITE(30,*) "Step",m,"Time(s)",t/v 
      WRITE(30,*) "Step",m,"Time(s)",t*v1
      WRITE(40,*) n_particle
      ! WRITE(40,*) "Step",m,"Time(s)",t/v 
      WRITE(40,*) "Step",m,"Time(s)",t*v1 
      WRITE(70,*) n_particle
      ! WRITE(40,*) "Step",m,"Time(s)",t/v 
      WRITE(70,*) "Step",m,"Time(s)",t*v1 
      DO j=1,n_particle
        WRITE(30,*) j,x(j),y(j),z(j)
        WRITE(40,*) j,xu(j),yu(j),zu(j)
        DO k=1,n_pos
          IF(pos(k)==j) THEN
            WRITE(70,*) j,k,dE(k)
          END IF
        END DO
      END DO
      ! WRITE(50,*) t/v,tmp_MSD*1e-20/n_particle,tmp_MSD*v*1e-20/(n_particle*t)
      ! WRITE(60,*) t/v,tmp_Tot_MSD*1e-20/n_particle,tmp_Tot_MSD*v*1e-20/(n_particle*t)
      ! WRITE(50,*) t/v,tmp_MSD*1e-20/n_particle
      WRITE(50,*) t*v1,tmp_MSD*1e-20/n_particle
      ! WRITE(60,*) t/v,tmp_Tot_MSD*1e-20/n_particle
      WRITE(60,*) t*v1,tmp_Tot_MSD*1e-20/n_particle
    END IF
  END DO

  ! Transition rate
  DO j=1,n_particle
    DO i=1,n_pos
      IF(pos(i)==j) THEN
        DO k=1,n_pos
          IF(k==i) THEN
            p_ik((j-1)*n_pos+k)=0.0
          ELSE
            IF(connect(i,k)==1) THEN
              ! Exclusion case
              IF(pos(k)==0) THEN
                !--20161012--
                ! dG=(dE(k)-dE(i))*alpha
                ! p_ik((j-1)*n_pos+k)=exp(-(dG+dG0)/(kB*Temp))
                dG=Eb(i,k)
                p_ik((j-1)*n_pos+k)=exp(-dG/(kB*Temp))
                !--20161012--
              ELSE
                p_ik((j-1)*n_pos+k)=0.0
              END IF
            ELSE
              p_ik((j-1)*n_pos+k)=0.0
            END IF
          END IF
        END DO
      END IF
    END DO
  END DO

  ! WRITE(100,*) "Step",m
  ! DO j=1,n_particle
  !   DO i=1,n_pos
  !     IF(pos(i)==j) THEN
  !       WRITE(100,*) "particle",j,"in pos",i
  !     END IF
  !   END DO
  !   DO i=1,n_pos
  !     IF(p_ik((j-1)*n_pos+i) /= 0.0) THEN
  !       WRITE(100,*) j,i,p_ik((j-1)*n_pos+i)
  !     END IF
  !   END DO
  ! END DO

  ! Transition rate
  DO j=2,n_particle*n_pos
    p_ik(j)=p_ik(j)+p_ik(j-1)
  END DO

  ! Hop
  CALL RANDOM_NUMBER(u1)  
  tmp_p=u1*p_ik(n_particle*n_pos)
  ! WRITE(100,*) "Random Number"
  ! WRITE(100,*) tmp_p
  ! WRITE(100,*) "------------------"
  IF(tmp_p <= p_ik(1)) THEN
    hop_particle=1
    hop_pos=1
    DO i=1,n_pos
      IF(pos(i)==hop_particle) THEN
        pos(i)=0
        pos(hop_pos)=hop_particle
        CALL RANDOM_NUMBER(u2)
        u2=1-u2
        dt=log(1/u2)/p_ik(n_particle*n_pos)
        WRITE(200,*) dt
        ! dt=1/p_ik(n_particle*n_pos)
        t=t+dt
        GO TO 100
      END IF
    END DO
!  ELSEIF(tmp_p > p_ik(n_particle*n_pos-1) .AND. tmp_p <= p_ik(n_particle*n_pos)) THEN
  ELSE
    DO j=2,n_particle*n_pos
      IF(tmp_p > p_ik(j-1) .AND. tmp_p <= p_ik(j)) THEN
        hop_particle=FLOOR(REAL(j)/REAL(n_pos))+1
        hop_pos=j-(hop_particle-1)*n_pos
        IF(hop_pos==0) THEN
          hop_particle=hop_particle-1
          hop_pos=n_pos
        END IF
        DO i=1,n_pos
          IF(pos(i)==hop_particle) THEN
            pos(i)=0
            pos(hop_pos)=hop_particle
            CALL RANDOM_NUMBER(u2)
            u2=1-u2
            dt=log(1/u2)/p_ik(n_particle*n_pos)
            WRITE(200,*) dt
            ! dt=1/p_ik(n_particle*n_pos)
            t=t+dt
            GO TO 100
          END IF
        END DO
      END IF
    END DO
  END IF

100 CONTINUE

END DO
CLOSE(30)
CLOSE(40)
CLOSE(50)
CLOSE(60)
CLOSE(70)

! WRITE(*,*) t,dt

! Write the Final-position.dat
WRITE(*,*) "Write Final-position.dat"
OPEN(10,FILE='Final-position.dat')
  WRITE(10,*) "# n_pos"
  WRITE(10,*) n_pos
  WRITE(10,*) "# n_particle"
  WRITE(10,*) n_particle
  WRITE(10,*) "# position, particle, energy"
  DO i=1,n_pos
    WRITE(10,*) i,pos(i),dE(i)
  END DO
CLOSE(10)

! Write Final Periodic Box
WRITE(*,*) "WRITE Final-Periodic-box.dat"
OPEN(10,FILE='Final-Periodic-box.dat')
  WRITE(10,*) "Time(s)"
  ! WRITE(10,*) t/v
  WRITE(10,*) t*v1
  WRITE(10,*) "# Particle NO, nx, ny, nz"
  DO i=1,n_particle
    WRITE(10,*) i,nx(i),ny(i),nz(i)
  END DO
CLOSE(10)

END PROGRAM main

! SUBROUTINE for random seed
SUBROUTINE init_random_seed()
  INTEGER :: i, n, clock
  INTEGER, DIMENSION(:), ALLOCATABLE :: seed

  CALL RANDOM_SEED(size = n)
  ALLOCATE(seed(n))

  CALL SYSTEM_CLOCK(count=clock)
  
  seed=clock+37*(/(i-1, i=1, n)/)
  CALL RANDOM_SEED(put=seed)

  DEALLOCATE(seed)
END SUBROUTINE init_random_seed
