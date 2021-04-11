SUBROUTINE CELLV
	USE VARIABLES
	INTEGER			  :: P,Q
	INTEGER			  :: Count1,Count2
	DOUBLE PRECISION :: XI,YI,XF,YF
	DOUBLE PRECISION :: DET,A,B,C
	DO 500 N=1,NC_D
		Count2=0
		DO 550 P = 4,5
			DO 525 Q = 1,2
				Count1=0
				XI = CG(Q,N)
				YI = CG(P,N)
				XF = OB(2)
				YF = YI		
				A = YF-YI
				B = XF-XI
				C = B*YI-A*XI

				DO 515 K=1,TNS
					DET = SURF(5,K)*B-A*SURF(6,K)  
					IF(DET.NE.0.) THEN
						XC = (SURF(6,K)*C-B*SURF(7,K))/DET
						YC = (SURF(5,K)*C-A*SURF(7,K))/DET				
						IF(((XC-MNMX(1,K)).GE.-TOL).AND.((XC-MNMX(2,K)).LE.TOL).AND. &
						((YC-MNMX(3,K)).GE.-TOL).AND.((YC-MNMX(4,K)).LE.TOL).AND. &
						((XC-XI).GE.-TOL).AND.((XC-XF).LE.TOL).AND. &
						((YC-YI).GE.-TOL).AND.((YC-YF).LE.TOL))THEN			
							Count1 = Count1+1
						END IF
					END IF
515	 		CONTINUE
				IF(MOD(Count1,2).NE.0) THEN
					Count2=Count2+1
				END IF
525	 	CONTINUE
550 	CONTINUE	
	CC(N)=(4-Count2)*CG(3,N)*CG(6,N)/4
500 CONTINUE
RETURN
END SUBROUTINE
