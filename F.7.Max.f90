SUBROUTINE MAX(I)
USE VARIABLES
INTEGER	:: I
DOUBLE PRECISION	:: B0,B1,B2,C,VTR,VNR
DOUBLE PRECISION	:: R1,R2,R3,R4

        L	= PP(9,I)
	
	B0	= RF(rank)
	IF (B0.GT.Ac_Max(Bod)) THEN
			Vnor = -Vnor
			Vtan = Vtan
			PP(4,I) = (Vtan*CO)-(Vnor*SI)
			PP(5,I) = ((Vtan*SI)+(Vnor*CO))
			PP(6,I) = PP(6,I)
	ELSE  
		  	R1=RF(rank)
			R2=RF(rank)
			R3=RF(rank)
			R4=RF(rank)
			C=SQRT(2.*BOLTZ*BODY_T(Bod)/SP(5,L))
			VTR=COS(2*PI*R1)*SQRT(-LOG(R2))*C
			VNR = ABS(SIN(2*PI*R1)*SQRT(-LOG(R2))*C)
			PP(4,I) = (VTR*CO)-(VNR*SI)
			PP(5,I) = ((VTR*SI)+(VNR*CO))
			PP(6,I)=COS(2*PI*R3)*SQRT(-LOG(R4))*C
		
			IF(RF(rank).LT. ALFA_INT) THEN
				CALL REFINDF(BODY_T(Bod),L)
				PP(7,I)=ERS
				PP(8,I)=EVS
			END IF
	END IF

END SUBROUTINE
