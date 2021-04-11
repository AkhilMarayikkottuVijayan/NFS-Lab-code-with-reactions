SUBROUTINE KSTEST
USE VARIABLES
DOUBLE PRECISION :: TEMP1,TEMP2,MPS,MAX_V,MIN_V,MAXWELL,COMP
DOUBLE PRECISION :: DIFF,M_CDF,C_CDF 
INTEGER :: TOTB,I,N,J

MIN_V = -10000.0
MAX_V = 10000.0
TOTB = (MAX_V-MIN_V)/DELU

DO 200 N=1,NC_D
	TEMP1=0.0d0 
	TEMP2=0.0d0 
	NEQ_PARAM(N) = 0.0d0
	MPS = 2.0*BOLTZ*CT(N)/SP(5,1)
	DO I=1,TOTB 
		COMP = NBIN(N,I)/NMCT(N)
                C_CDF = TEMP1+COMP
		TEMP1 = C_CDF

		VMAX = MIN_V + I*DELU
		MAXWELL =  ((1.0/(SQRT(PI*MPS)))*EXP(-(VMAX**2.0)/MPS))*DELU
		M_CDF = TEMP2+MAXWELL
		TEMP2 = M_CDF
		DIFF = ABS(C_CDF-M_CDF)
		IF(DIFF.GT. NEQ_PARAM(N)) NEQ_PARAM(N) = DIFF
	END DO
	IF(NEQ_PARAM(N) .GT. KS_CRIT) THEN
		CELLSPOT(N) = 0
	ELSE 
		CELLSPOT(N) = 1
	END IF
200 CONTINUE

END SUBROUTINE
