SUBROUTINE REROT(N)
USE VARIABLES
DOUBLE PRECISION :: ERM

DO 200 I =1,2
	IF(R_FLAG(I).EQ.1)THEN
		L = PP(9,CA(I))
                E_Redis    = E_Redis+PP(7,CA(I))
		IF(SP(7,L).EQ.2)THEN
			ERM = 1.-RF(rank)**(1./RM(2))
		ELSE
			RM(1) = 0.5*SP(7,L)
			CALL LBS(RM(1)-1.,RM(2)-1.,ERM)
		END IF
		PP(7,CA(I)) = ERM*E_Redis	
		E_Rotat(2)= PP(7,CA(I))
		E_Redis	 = E_Redis-E_Rotat(2)
	END IF
200 CONTINUE

E_Rotat(2) = 0

DO 300 I =1,2
	IF(R_FLAG(I).EQ.1)THEN
		L = PP(9,CA(I))
		E_Rotat(2) = E_Rotat(2)+PP(7,CA(I))
	END IF
300 CONTINUE
END SUBROUTINE
