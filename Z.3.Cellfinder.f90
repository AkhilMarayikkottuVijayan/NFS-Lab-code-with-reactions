SUBROUTINE CELLFINDER(I,X,Y)
USE VARIABLES
DOUBLE PRECISION :: X,Y
INTEGER :: MC,MCX,MCY,MSC,MSCX,MSCY
INTEGER :: A,B,I,CHK,II,LP
		A=X/CW+1
		B=Y/CH-(rank*NCY_D)
		MC=(B*NCX_D)+A
		IF(ADP.EQ.1)THEN
			IF(CAR(MC).EQ.1) THEN
				DO 100 K = CAB(MC),CAB(MC)+(4**FLAG(6))				
					IF(X .GE.CG(1,K).AND.X.LT.CG(2,K)) THEN
						IF(Y .GE. CG(4,K).AND.Y.LT.CG(5,K))THEN
							MC = K 
							GO TO 10
						END IF
					END IF
100			CONTINUE
			ELSE
				MC=CAB(MC)
			END IF
		END IF
	
10		MSCX = ((X-CG(1,MC))/CG(3,MC))*(NSCX-.001)+1
		MSCY = ((Y-CG(4,MC))/CG(6,MC))*(NSCY-.001)+1
		MSC = (MSCY-1)*NSCX+MSCX+NSCX*NSCY*(MC-1)
		IF (MSC.LT.1) MSC = 1
		IF (MSC.GT.NSC_D) MSC = NSC_D
		IP(I) = MSC
IF(CC(MC).EQ.0)MSTAT(I)=3
RETURN
END SUBROUTINE
