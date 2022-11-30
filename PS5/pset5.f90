! ************************************************
      SUBROUTINE ELOSS(ENE,ANGL,ENEAFT)
      COMMON IX,ANUMBE
! *** CALCULATE ENERGY LOSS IN COLLISION, USE ISOTROPIC (S-WAVE) 
! *** APPROXIMATION. ROUND UP ANY ENERGY BELOW 0.1 EV TO 0.02 EV (THERMAL)
      IF(ENE.LT.1.E-7)THEN
      ENEAFT=2.E-8
      ELSE
! *** THIS IS THE MAXIMUM RECOIL ENERGY THAT CAN BE IMPARTED
      EREMAX=((4.*ANUMBE)/((1.+ANUMBE)**2.))*ENE
      CALL RANDM(RN1)
! *** THE PROBABILITY DISTRIBUTION FOR RECOIL ENERGIES IS FLAT
! *** FROM ZERO UP TO THE MAXIMUM 
      EREMAX=EREMAX*RN1
      ENEAFT=ENE-EREMAX
      ENDIF

      IF(ENE.LT.1.E-7)THEN
      CALL RANDM(RN1)
      ANGL=ACOS((2.*RN1)-1.)
      ELSE
      ETA=ACOS(SQRT((EREMAX/ENE)*(((1.+ANUMBE)**2.)/(4.*ANUMBE))))
      ANGL=ATAN((SIN(2.*ETA))/((1./ANUMBE)-COS(2.*ETA)))
      ENDIF
      
      RETURN
      END
! ************************************************
      SUBROUTINE ENERGY(ENE)
      COMMON IX
! *** USE VON NEUMANN'S METHOD ("HIT AND MISS") TO CALCULATE THE 
! *** ENERGY OF THE NEUTRON EMITED FROM A FISSION SOURCE.
   7  CALL RANDM(RN1)
! *** P=0 ABOVE ~10 MEV
      RN1=RN1*9.999
! *** APPLY MAXWELL SPECTRUM
      PROB=SQRT(RN1)*EXP(-RN1/1.4)
      CALL RANDM(RN2)
! *** THE MAXIMUM OF THE DISTRIBUTION AS WRITTEN IS LESS THAN 0.5 
      RN2=RN2*0.5
      IF(RN2.GT.PROB)GOTO 7    
      ENE=RN1   
      RETURN
      END
! ************************************************
      SUBROUTINE RANDM(RN1)
      COMMON IX
      IY=IX*6539
      IF (IY < 0) IY=IY+2147483647+1
      IF (IY >= 0) RN1 = IY
      RN1=RN1*.4656613E-9
      IX=IY
      RETURN
      END
! ************************************************
      SUBROUTINE XSECT(ENE,XELAST,XABSOR)
! *** RETURNS THE APPROXIMATE CROSS SECTIONS (BARNS) FOR ABSORPTION AND 
! *** ELASTIC SCATTERING IN CARBON.
! *** HERE WE NEED THE ENERGY IN EV, NOT MEV
      X=ENE*1.E6

! *** CHECK THESE APPROXIMATIONS VS THE PLOTS IN THE HANDOUTS
      IF(X.LT.1.E4)THEN
      XELAST=5.
      ELSE
      XELAST=10.5-(1.346*LOG10(X))
      ENDIF
      XELAST=XELAST*1.E-24

      IF(X.LT.1.E3)THEN
      XABSOR=(6.442E-4)*(X**(-0.49421))
      ELSE IF(X.LT.1.E5) THEN
      XABSOR=1.5E-5
      ELSE IF(X.LT.5.E6) THEN
      XABSOR=2.E-5
      ELSE
      XABSOR=(4.E-06)*EXP(X*3.2189E-07) 
      ENDIF
      XABSOR=XABSOR*1.E-24
      RETURN
      END
!*************************************************
      SUBROUTINE EULER(EX,EY,EZ,ANGL,SX,SY,SZ)
!     THIS SUBROUTINE TAKES THE ORIGINAL LINEAR TRAJECTORY,
!     ROTATES IT TO LIE ALONG THE Z-AXIS, GENERATES A VECTOR
!     AT ZENITH ANGLE THETA = SCATTERING ANGLE 
!     AND AZIMUTHAL ANGLE FI = RANDOM * 2PI. THE
!     ORIGINAL AXIS IS NOW ROTATED BACK TAKING THE SCATTERING
!     VECTOR WITH IT. NOW WE HAVE THE SCATTERED
!     DIRECTION VECTOR (SX,SY,SZ).
!     WE USE EULER ANGLES TO PERFORM THE TRANSFORMATION.
      COMMON IX
! *** NORMALIZE THE DIRECTION TO A UNIT VECTOR (IN CASE IT WASN'T)
      S=SQRT(EX**2+EY**2+EZ**2)
      EX=EX/S
      EY=EY/S
      EZ=EZ/S
      BET=ACOS(EZ)
! *** THESE APPROXIMATIONS ARE ONLY NEEDED FOR COMPTON SCATTERING 
! *** FOR GAMMAS (BUT THEY WILL NOT HURT HERE)
      IF(ABS(BET).LT.0.027)ALF=0.0
      IF(ABS(BET).LT.0.027)GO TO 44
      ARG=EY/SIN(BET)
      AARG=ABS(ARG)
      IF(AARG.LT.1.0)GOTO 344
      ARG=ARG/(1.0001*AARG)
344   ALF=ASIN(ARG)
 44   CONTINUE
      SCO1=COS(ALF)*SIN(BET)+EX
      SCO1=ABS(SCO1)
      SCO2=ABS(EX)
      IF(SCO1.LT.SCO2)BET=-BET
      IF(SCO1.LT.SCO2)ALF=-ALF
      GAM=0.0
!     WE NOW HAVE THE EULER ANGLES OF ROTATION BETWEEN
!     Z-AXIS TO DIRECTION OF INITIAL PARTICLE.
      THET = ANGL
      CALL RANDM(RN1)
      FI = 6.2831853 * RN1
!     WE NOW HAVE SCATTERED THE PARTICLE FROM THE
!     Z-AXIS AND MUST ROTATE IT TO THE ORIGINAL UNSCATTERED
!     PARTICLE DIRECTION. CACULATE THE ROTATION MATRIX.
      R11 = COS(ALF)*COS(BET)*COS(GAM)-SIN(ALF)*SIN(GAM)
      R12 = COS(BET)*SIN(ALF)*COS(GAM)+COS(ALF)*SIN(GAM)
      R13 =-SIN(BET)*COS(GAM)
      R21 =-SIN(GAM)*COS(BET)*COS(ALF)-SIN(ALF)*COS(GAM)
      R22 =-SIN(GAM)*COS(BET)*SIN(ALF)+COS(ALF)*COS(GAM)
      R23 = SIN(BET)*SIN(GAM)
      R31 = SIN(BET)*COS(ALF)
      R32 = SIN(ALF)*SIN(BET)
      R33 = COS(BET)
      SOX = SIN(THET)*COS(FI)
      SOY = SIN(THET)*SIN(FI)
      SOZ = COS(THET)
      SX =  R11*SOX+R21*SOY+R31*SOZ
      SY =  R12*SOX+R22*SOY+R32*SOZ
      SZ =  R13*SOX+R23*SOY+R33*SOZ
!     WE NOW HAVE THE UNIT PROPAGATION VECTOR OF THE
!     SCATTERED PARTICLE IN THE *ORIGINAL* FRAME.
      RETURN
      END
! ************************************************

program main
!
common ix, anumbe

real L ! the length of the moderator, cm 
real x, y, z  ! the coordinates of the neutron
real vx, vy, vz ! the unit vector of the velocity
real sx, sy, sz ! the unit vector after scattering

real dnext ! the distance to the next interaction

! measured in MeV
real ene   ! the energy of the neutron (cant be called 'energy')
real eneaft ! energy after the collision
real xelast, xabsor ! cross sections for elastic scattering and 
real prob_absorb ! probability that the neutron was absorbed
! absoroption

! var to hold random number
real rn1

integer n, i ! loop counters

integer neutron_count ! number of neutrons to simulate

! integer counters for back-scattered, traversed, thermalized
! adn absorbed neutrons
integer back_scat, trav, therm, absorb
real perc_scat, perc_trav,perc_therm,perc_absorb
double precision PI

! number used to normalize 
real norm

! ix = random seed for generator
! anumbe = molar mass of carbon in au (from formulas used in slides)
ix = 777 
anumbe = 12.011

neutron_count = 100000 
PI = 4.D0*ATAN(1.D0)

! reset the counts
back_scat = 0
trav = 0
therm = 0
absorb = 0

call execute_command_line('> neutron_data') ! clear file
open(10,file='neutron_data',status="unknown",position='append')
write(10,*)"L,Trans,Therm,Back,Absorb" ! all but L are percents

print *, "checkpoint A"

! loop over desired legnths (multiplied by 10 so that I can use ints)
do n = 10, 500, 5
! local length of the separation, (cm)
L = real(n) / 10.

! loop over each neutron
do i = 1, neutron_count
! reset coordinates (all in cm)
x = 0.
y = 0.
z = 0. ! L is measured along Z 



! get new randomized energy (MeV)
call energy(ene)

! It is important to initialize velocities this way in
! order to give a fair distribution in any direction
!**************
! get random direction vector 
call randm(angle1) ! theta
call randm(angle2) ! phi
angle1 = 2 * PI * angle1 
angle2 = PI * angle2 ! adapt to unit cartesian vector 
vx = cos(angle2) * sin(angle1)
vy = sin(angle2) * sin(angle1)
vz = cos(angle1)
! assert that we are starting in the positive z-direction
vz = abs(vz)
! normalize the vector 
norm = sqrt(vx**2 + vy**2 + vz**2) 
vx = vx/norm
vy = vy/norm
vz = vz/norm
!**************

!print *, "checkpoint B"
! come back to this line if the neutron survives
10 continue

! call xsect to get cross sections --> relative probabilities
! of scattering or absorbtion
call xsect(ene, xelast, xabsor)

! determine dnext (cm) using the values from the slides
! figure out how to get dnext from cross sections
call randm(rn1)
! recalulate this using the sigma formula used in the slides, 
! where sigma total = xelast + xabsorb 
dnext = - log(rn1) / ((xelast + xabsor) * 1.05e23)
! this should return a value in cm 

! calc the new positions 
x = x + (vx * dnext)
y = y + (vy * dnext)
z = z + (vz * dnext)

! check for backscattering 
if (z <= 0) then
        back_scat = back_scat + 1 
        ! jump to the next neutron
        cycle  
endif

! check if it made it across 
if (z >= L) then
        trav = trav + 1 
        ! double check if this is the right units
        if (ene*1.E6 < 500) then
                therm = therm + 1
        endif
        cycle
endif

! check the relative probability for absorption
! change this back to the original expression sent in the email
prob_absorb = xabsor / (xabsor + xelast)
call randm(rn1)
 
if (rn1 <= prob_absorb) then
        absorb = absorb + 1 
        cycle
endif

! below this line means that the neutron is still in
! the moderator and has not been absorbed 

call eloss(ene, angl, eneaft)
! new energy after the collision
ene = eneaft
call euler(vx,vy,vz,angl,sx,sy,sz)
vx = sx
vy = sy
vz = sz



goto 10


! end neutron loop
enddo

if (mod(int(L), 5) == 0) print *, int(L)
! write to file
perc_scat = real(back_scat) / real(neutron_count)
perc_trav = real(trav) / real(neutron_count)
perc_therm = real(therm) / real(neutron_count)
perc_absorb = real(absorb) / real(neutron_count)
write(10,*)L,",",perc_trav,",",perc_therm,",",perc_scat,",",perc_absorb

! reset the counts for the next cycle
back_scat = 0
trav = 0
therm = 0
absorb = 0




! end length loop
enddo 



! want to test if the above subroutines can be used with fortran 90
print *, "runtime completed" 

close(10)
stop
end program main
