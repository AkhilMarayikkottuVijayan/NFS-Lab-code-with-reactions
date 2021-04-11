SUBROUTINE MOVE
USE VARIABLES
 DOUBLE PRECISION :: XI,YI,X,Y
 INTEGER :: L
 SC(1:2) =0
 RC(1:2) =0
CountTr=0

 CALL JETENTER

	DO 100 I = 1, NM
SE_prev =0
		AT = DTM
		IF(MSTAT(I).EQ.2)AT = RF(rank)*DTM
10		MOVT=MOVT+1	
		FLAG(4) = 0
		XI = PP(1,I)
		YI = PP(2,I)
		X  = XI+(AT*PP(4,I))
		Y  = YI+(AT*PP(5,I))
		
			IF(((X.GT.OVER(1)).AND.(X.LE.OVER(2))))THEN
				IF((Y.GT.OVER(3)).AND.(Y.LE.OVER(4)))THEN
					CALL REFCOND(I,XI,YI,X,Y)

					IF((FLAG(4).NE.0).AND.(SE_Prev.NE. SE))THEN
	SE_Prev = SE
	L		=	PP(9,I)
	Bod	= BD(SE)
	SI		= Sin(SURF(10,SE))
	CO		= Cos(SURF(10,SE))
	Vnor	= -(PP(4,I)*SI)+(PP(5,I)*CO)
	Vtan	= (PP(4,I)*CO)+(PP(5,I)*SI) 
	Vres	= SQRT(PP(4,I)**2+PP(5,I)**2+PP(6,I)**2)	
	IF(NPR.GE.NTS(1))THEN
		CSS(1,SE) 	 = CSS(1,SE)+1
		CSS(2,SE) 	 = CSS(2,SE)+SP(5,L)*Vnor
		CSS(4,SE) 	 = CSS(4,SE)+SP(5,L)*Vtan
		CSS(6,SE)	 = CSS(6,SE)+0.5*SP(5,L)*Vres**2	
	END IF
						PP(1,I) = XC
						PP(2,I) = YC
						L=PP(9,I)
			
			ETS=0.5*(SP(5,L)*(Vres**2))
			
		IF(NPR.GT.NTS(1))	QI(SE)=QI(SE)+ETS+EVS+ERS
					IF(GSM(Bod).EQ."MAX")CALL MAX(I)
					IF(GSM(Bod).EQ."CLL")CALL CLL(I)
	Vnor	= -(PP(4,I)*SI)+(PP(5,I)*CO)
	Vtan	= (PP(4,I)*CO)+(PP(5,I)*SI) 
	Vres	= SQRT(PP(4,I)**2+PP(5,I)**2+PP(6,I)**2)	
	
		
	IF(NPR.GE.NTS(1))THEN
		CSS(3,SE)	 = CSS(3,SE)+SP(5,L)*Vnor
		CSS(5,SE) 	 = CSS(5,SE)+SP(5,L)*Vtan
		CSS(7,SE) 	 = CSS(7,SE)+0.5*SP(5,L)*Vres**2
	END IF
			ETS=0.5*(SP(5,L)*(Vres**2))
			IF(NPR.GT.NTS(1))QR(SE)= QR(SE)+ETS+EVS+ERS
		
						GO TO 10
				END IF

			END IF
		END IF
		PP(1,I) = X
		PP(2,I) = Y
		
		CALL BOUNDS(I,PP(1,I),PP(2,I))
		CALL PBOUNDS(I,PP(1,I),PP(2,I))
		IF((MSTAT(I).NE.1).AND.(MSTAT(I).NE.4))CALL CELLFINDER(I,PP(1,I),PP(2,I))
	
100 CONTINUE

	CALL PURGE

END SUBROUTINE
