SUBROUTINE PCK(XI,YI)
	USE VARIABLES
	INTEGER			  :: CountX,CountY
	DOUBLE PRECISION :: XI,YI,XF,YF
	DOUBLE PRECISION :: DET,A,B,C
	
10	FLAG(1) = 0
	CountX = 0 
	CountY = 0
	
		XF = OB(2)
		YF = YI		
		A = YF-YI
		B = XF-XI
		C = B*YI-A*XI
		
		DO 100 K=1,TNS
			DET = SURF(5,K)*B-A*SURF(6,K)  
			IF(DET.NE.0.) THEN
				XC = (SURF(6,K)*C-B*SURF(7,K))/DET
				YC = (SURF(5,K)*C-A*SURF(7,K))/DET
				
				IF(((XC-MNMX(1,K)).GE.-TOL).AND.((XC-MNMX(2,K)).LE.TOL).AND. &
					((YC-MNMX(3,K)).GE.-TOL).AND.((YC-MNMX(4,K)).LE.TOL).AND. &
					((XC-XI).GE.-TOL).AND.((XC-XF).LE.TOL).AND. &
					((YC-YI).GE.-TOL).AND.((YC-YF).LE.TOL))THEN			
						CountX = CountX+1
				END IF
			END IF
100 	CONTINUE


		XF = XI
		YF = OB(4)		
		A = YF-YI
		B = XF-XI
		C = B*YI-A*XI

		DO 200 K=1,TNS
			DET = SURF(5,K)*B-A*SURF(6,K)  
			IF(DET.NE.0.) THEN
				XC = (SURF(6,K)*C-B*SURF(7,K))/DET
				YC = (SURF(5,K)*C-A*SURF(7,K))/DET
				
				IF(((XC-MNMX(1,K)).GE.-TOL).AND.((XC-MNMX(2,K)).LE.TOL).AND. &
					((YC-MNMX(3,K)).GE.-TOL).AND.((YC-MNMX(4,K)).LE.TOL).AND. &
					((XC-XI).GE.-TOL).AND.((XC-XF).LE.TOL).AND. &
					((YC-YI).GE.-TOL).AND.((YC-YF).LE.TOL))THEN			
						CountY = CountY+1
				END IF
			END IF
200 	CONTINUE
		

		IF(MOD(CountX,2).NE.0) THEN
			FLAG(1)=1
		ELSE IF(MOD(CountY,2).NE.0) THEN
			FLAG(1)=1
		END IF
		IF(FLAG(1).EQ.1) THEN
			XI = CB(1)+RF(rank)*(CB(2)-CB(1))
			YI = CB(3)+RF(rank)*(CB(4)-CB(3))
			GO TO 10
		END IF
RETURN
END SUBROUTINE
