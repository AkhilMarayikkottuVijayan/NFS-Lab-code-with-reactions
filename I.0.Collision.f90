SUBROUTINE COLLISION
USE VARIABLES
INTEGER :: NSEL
INTEGER :: N,L,LL,Q,KKK,SN
DOUBLE PRECISION::A,SELT,PRO
FLAG(5) = 0
RCM=0
PRO=0.0d0
SELT = 0.0d0

	DO 100 N = 1, NC_D
		SN = 0
		DO L=1,NSP_T
                  IF(SP_EX(L).EQ.1)SN=SN+CS(1,N,L)
                END DO
		   IF(NSMP.GT.0.AND.CC(N).GT.0)THEN
        	     A= SN*FNUM/(CC(N)*NSMP)!What is CC
                   END IF

		DO 110 L = 1, NSP_T
			DO 111 LL = 1, NSP_T
				IF((SP_EX(L).EQ.1).AND.(SP_EX(LL).EQ.1))THEN
					CALL NCOLLS(N,L,LL,NSEL) 
					CALL OCOLLS(N,L,LL,NSEL)
					IF (FLAG(5).EQ.1)THEN
						CVM=CARC(1,N,L,LL) 
						SELT=SELT+NSEL
						DO 112 Q = 1, NSEL
							CALL SCOLLS(N,L,LL)
							IF(CVR.GT.CVM) THEN
									CVM=CVR
									CARC(1,N,L,LL) = CVR
							END IF 				
							IF(RF(rank).LT.(CVR/CARC(1,N,L,LL))) THEN 		
								NCOL(N)=NCOL(N)+1				
								COL(L,LL)=COL(L,LL)+1			
								COL(LL,L)=COL(LL,L)+1
								CALL ICOLLS(N)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              IF(MVV.EQ.0)THEN
                  IF((FLAG(9).EQ.1).AND.(SWITCH_R(L,LL).EQ.1))THEN
                    IF(CT(N).GT.0)THEN ! Number of particle in cell tag N
                        CUMU_PROB(L,LL,N) =CUMU_PROB(L,LL,N)+RECOM_COEFF(L,LL)*A*CT(N)**RECOM_TEMP_EXP(L,LL)
                         IF(N.EQ.2000)WRITE(*,*) N, CUMU_PROB(L,LL,N)
                          IF(CUMU_PROB(L,LL,N).GT.1)THEN
                              RCM = RCM+1
                              RPAIR(RCM,1) = CA(1)
                              RPAIR(RCM,2) = CA(2)
                                CUMU_PROB(L,LL,N) =CUMU_PROB(L,LL,N)-1.0d0
                           END IF
                     END IF
                  END IF
                END IF
                 
								CALL ECOLLS(N)
							END IF
112					END DO 
					ELSE
						CARC(2,N,L,LL)=CARC(2,N,L,LL)+ASEL
					END IF
				END IF
111 		CONTINUE
110 	CONTINUE	
100 CONTINUE

END SUBROUTINE
