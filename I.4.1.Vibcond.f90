SUBROUTINE VIBCOND(N)
USE VARIABLES
DOUBLE PRECISION :: V_Relax,COLT,A
V_FLAG(1:2) = 0
DO 100 I =1,2
	IF(I.EQ.1) THEN
		L = PP(9,CA(1)) 
		LL= PP(9,CA(2)) 
	ELSE
		L = PP(9,CA(2)) 
		LL= PP(9,CA(1)) 
	END IF

	IF(SP(8,L).GT.0) THEN
		IF (SPM(14,L,LL).GT.0.) THEN
			IF (SVIB(N,L).GT..0001) THEN
                          COLT=(E_Trans(1)+PP(8,CA(I)))/((RM(2)+0.5*SVIB(N,L))*BOLTZ)
                        ELSE
	        	  COLT=CT(N)
                END IF
                    
                      A=SPM(14,L,LL)*(COLT**(-0.33333))
			IF (A.LT.50.) THEN
				V_Relax=(SPM(13,L,LL)/(COLT**SPM(3,L,LL)))*EXP(A)
			ELSE
				V_Relax=1.E7
			END IF
		ELSE
			V_Relax=SPM(13,L,LL)
		END IF
		
		IF((1/V_Relax).GT.RF(rank))THEN
			V_FLAG(I) = 1
		END IF
	END IF
100 CONTINUE
END SUBROUTINE
