SUBROUTINE JETINIT
USE VARIABLES

ALLOCATE(A_con(2,1:NJET),B_con(2,1:NJET),C_con(2,1:NJET))

 
	DO 100 J=1, NJET
		JET(5,J) = SQRT((JET(2,J)-JET(1,J))**2+(JET(4,J)-JET(3,J))**2)
		JET(6,J) = ATAN2((JET(4,J)-JET(3,J)),(JET(2,J)-JET(1,J)))
	
		IF(JET(6,J).LT.0)JET(6,J)=JET(6,J)+(2*PI)
		JET(7,J) = JET(6,J)+PI/2
		IF(JET(7,J).GT.(2*PI)) JET(7,J)=JET(7,J)-(2*PI)

		VP_J(2,J) = VP_J(1,J)*COS(JET(7,J)) 
		VP_J(3,J) = VP_J(1,J)*SIN(JET(7,J))
LL=0	
		DO 110 L=1, NSP_T
			VMP_Local = SQRT(2.*BOLTZ*TMP_J(J)/SP(5,L)) 
			SCJ = VP_J(1,J)/VMP_Local
			IF (ABS(SCJ).LT.10.1) A = (EXP(-SCJ*SCJ)+SPI*SCJ*(1.+ERF(SCJ)))/(2.*SPI)
			IF (SCJ.GT.10.)       A =  SCJ
			IF (SCJ.LT.-10.)      A =  0. 
			ENT_J(J,L) = FND_J(J)*SP_FRJ(J,L)*A*VMP_Local*DTM*JET(5,J)/FNUM 
110	CONTINUE

	IF(PROF_J(J).EQ."LINEAR")THEN
		A_con(1,J) = 0
		B_con(1,J) = (VP_J(1,J)-VP_J(6,J))/(VP_J(4,J))
		C_con(1,J) = VP_J(6,J)
		A_con(2,J) = 0
		B_con(2,J) = (VP_J(1,J)-VP_J(5,J))/(VP_J(4,J)-JET(5,L))
		C_con(2,J) = VP_J(5,J)-(B_con(2,J)*JET(5,L)) 
	ELSE IF(PROF_J(J).EQ."PARABOLIC")THEN
		A_con(1,J) = (VP_J(6,J)-VP_J(1,J))/(VP_J(4,J))**2
		B_con(1,J) = -2*A_con(1,J)*VP_J(4,J)
		C_con(1,J) = VP_J(6,J)
		A_con(2,J) = (VP_J(5,J)-VP_J(1,J))/(JET(5,L)-VP_J(4,J))**2
		B_con(2,J) = -2*A_con(2,J)*VP_J(4,J)
		C_con(2,J) = VP_J(5,J)-(A_con(2,J)*JET(5,L)**2)-(B_con(2,J)*JET(5,L)) 
	ELSE
		A_con(1,J) = 0
		B_con(1,J) = 0
		C_con(1,J) = VP_J(1,J)
		A_con(2,J) = 0
		B_con(2,J) = 0
		C_con(2,J) = VP_J(1,J)
	END IF
100 CONTINUE
IF(rank.EQ.0)WRITE(*,*)"JET INITIALIZATION .......... SUCCESSFUL"
END SUBROUTINE

