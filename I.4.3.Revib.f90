SUBROUTINE REVIB(N)
USE VARIABLES

DOUBLE PRECISION :: RRRR
DO 200 I =1,2
	IF(V_FLAG(I).EQ.1) THEN
		L = PP(9,CA(I))
		MAXLVL=E_Redis/(BOLTZ*SP(9,L))
10		PP(10,CA(I)) = RF(rank)*MAXLVL	
		E_Vibra(2) = INT(PP(10,CA(I)))*BOLTZ*SP(9,L)
		Prob = (1-E_Vibra(2)/E_Redis)**(RM(2)-1)		
		IF (Prob.LT.RF(rank)) GO TO 10
		E_Redis	 = E_Redis-E_Vibra(2)
		PP(8,CA(I))	= E_Vibra(2)
	END IF
200 CONTINUE
	
E_Vibra(2) = 0.0d0

RRRR = E_Redis +PP(8,CA(1))+PP(8,CA(2))

DO 300 I =1,2
	IF(V_FLAG(I).EQ.1) THEN
		E_Vibra(2) = E_Vibra(2)+PP(8,CA(I))
	END IF
	
300 CONTINUE
END SUBROUTINE
