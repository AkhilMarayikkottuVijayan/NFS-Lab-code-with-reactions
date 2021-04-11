SUBROUTINE ICOLLS(N)
USE VARIABLES	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! inelastic collision conditions and redistribution subroutines are claled here!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

L  = PP(9,CA(1))
LL = PP(9,CA(2))

E_Trans(1) = 0.5*SPM(5,L,LL)*VRR
E_Rotat(1) = PP(7,CA(1))+PP(7,CA(2))
E_Vibra(1) = PP(8,CA(1))+PP(8,CA(2))
!E_Vibra(1) = INT(PP(10,CA(1)))*BOLTZ*SP(9,L)+INT(PP(10,CA(2)))*BOLTZ*SP(9,LL)

E_Total = E_Trans(1)+E_Rotat(1)+E_Vibra(1)

RM(2) = 2.5-SPM(3,L,LL)
IF(rank.eq.0 .AND. PP(8,CA(1)).GT.0.00001) WRITE(*,*)PP(8,CA(1)),PP(10,CA(1))
DISS_MARK = 0
EXCH_MARK = 0
CALL VIBCOND(N)
CALL ROTCOND(N)

E_Redis	   = E_Trans(1)
E_Rotat(1) = 0.0d0
E_Vibra(1) = 0.0d0
E_Trans(2) = 0.0d0
E_Rotat(2) = 0.0d0

        E_Vibra(1) = 0
        DO 101 I =1,2
                IF(V_FLAG(I).EQ.1) THEN
                        E_Vibra(1) = E_Vibra(1)+PP(8,CA(I))
               END IF
        101 CONTINUE
        E_Redis    = E_Redis+E_Vibra(1)
CALL DISSCOND
CALL EXCHCOND(N)
CALL REVIB(N)
CALL REROT(N)
 E_Trans(2) = E_Redis
CALL REDTRANS

END SUBROUTINE
