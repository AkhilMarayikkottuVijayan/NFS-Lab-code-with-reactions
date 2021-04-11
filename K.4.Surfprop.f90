SUBROUTINE SURFPROP
USE VARIABLES
USE MPI
INTEGER :: Q
DOUBLE PRECISION :: SURF_NO,SURF_PR,SURF_SH,SURF_EN,MACF,CD_P,CD_S
DOUBLE PRECISION :: XS,YS,DRAG_P,DRAG_S,LIFT,AREA,CD,CL,LL,STN1,STN2,STN3,Taw,R
CD=0.0d0
CL=0.0d0
CP=0.0d0
DRAG_P=0.0d0
LIFT=0.0d0
DRAG_S=0.0d0
DO K=1,TNS
DO Q=1,7
CALL MPI_REDUCE(CSS(Q,K),CSS_L(Q,K),1,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
END DO
CALL MPI_REDUCE(QR(K),Q_R(K),1,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
CALL MPI_REDUCE(QI(K),Q_I(K),1,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
END DO

IF (rank.EQ.0) THEN

        A = FNUM/(DTM*(NTS(2)-NTS(1)))
        !B = (NTS(2)*DTM)

        OPEN(10,FILE="3.5.Surf Properties.dat",FORM="FORMATTED",ACCESS="APPEND")
        AREA=0
        WRITE (10,*)'TITLE = "Surf Properties"'
        WRITE (10,*)'VARIABLES = "X (m)","Y (m)","Surf ID","Pres","Cp","Sh","Cf","Heat Flux","Ch1","Ch2","Ch3"'
        WRITE (14,*)" CD "," CD_P "," CD_S "," CL"
        DO 100 K=1,TNS
	        SI 	  = SIN((SURF(10,K)))  
	        CO 	  = COS((SURF(10,K)))
	        AREA	  = AREA + SURF(8,K)
	        SURF_NO = CSS_L(1,K)*A

	        SURF_PR = (CSS_L(3,K)-CSS_L(2,K))*A/(SURF(8,K))
	        CP=(SURF_PR-(FND_D*TMP_D*BOLTZ))/(0.5*DEN_EF*VP_J(1,1)**2.0)
	        SURF_SH = (CSS_L(4,K)-CSS_L(5,K))*A/(SURF(8,K))
	        CF=SURF_SH/(0.5*DEN_EF*VP_J(1,1)**2.0)
	        !KK=CSS_L(4,K)-CSS_L(5,K)
	        LL=CSS_L(2,K)-CSS_L(3,K)
	        SURF_EN = (CSS_L(6,K)-CSS_L(7,K))*SURF_NO

	        LIFT    = LIFT+(-SURF_PR*CO + SURF_SH*SI)*(SURF(8,K))
	        DRAG_P  = DRAG_P+SURF_PR*SI*(SURF(8,K))! + SURF_SH*CO)*(SURF(8,K))
	        DRAG_S  = DRAG_S+SURF_SH*CO*(SURF(8,K))
	        XS	= (SURF(3,K)+SURF(1,K))/2
	        YS      = (SURF(4,K)+SURF(2,K))/2

        !	QF(K)=(Q_I(K)-Q_R(K))*FNUM/(SURF(8,K)*B)
	        QF(K) = (Q_I(K)-Q_R(K))*A/(SURF(8,K))

	        MACF = VP_J(1,1)/(SQRT(GAMM*R_STREAM*TMP_J(1)))

	        Taw = TMP_J(1)*(1+((GAMM-1)/2)*MACF**2)

	        STN1 = QF(K)/(DEN_EF*VP_J(1,1)*CP_EF*(Taw-BODY_T(1)))
	        STN2 = QF(K)/(0.5*DEN_EF*VP_J(1,1)**3.0)
	        STN3 = QF(K)/(DEN_EF*VP_J(1,1)**3.0)
	
	        WRITE(10,*)XS,YS,K,SURF_PR,CP,SURF_SH,CF,QF(K),STN1,STN2,STN3
        100 CONTINUE
	
        CD_P = DRAG_P/(0.5*DEN_EF*VP_J(1,1)**2.0*CHARLEN)
        CD_S = DRAG_S/(0.5*DEN_EF*VP_J(1,1)**2.0*CHARLEN)
        CD = CD_S + CD_P
        CL = LIFT/(0.5*DEN_EF*VP_J(1,1)**2.0*CHARLEN)
        WRITE(14,*)CD,CD_P,CD_S,CL
        CLOSE(10)

END IF

END SUBROUTINE
