SUBROUTINE REDTRANS
USE VARIABLES
INTEGER 				:: L,LL
DOUBLE PRECISION  :: A
	L  = PP(9,CA(1)) 
	LL = PP(9,CA(2))
	A=SQRT(2.*E_Trans(2)/SPM(5,L,LL))
	IF (ABS(SPM(4,L,LL)-1.).LT.1.E-3) THEN
		VR=A
	ELSE
		DO 300 K=1,3
			VRC(K)=VRC(K)*A/VR
300	CONTINUE
		VR=A
	END IF
END SUBROUTINE
