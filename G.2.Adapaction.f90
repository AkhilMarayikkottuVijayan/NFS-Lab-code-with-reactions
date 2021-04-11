SUBROUTINE ADAPACTION
USE VARIABLES
INTEGER :: P,Q,R
DOUBLE PRECISION ::NCW,NCH
INTEGER :: LP,CHC
P=0
ADP =1
	DO 100 N=1,NC_D
		P=P+1
		IF(CAR(N).NE.0) THEN
			NCW = CG(3,N)/(2**FLAG(6))
			NCH = CG(6,N)/(2**FLAG(6))	
			DO 110 Q = 1,2**FLAG(6)
				DO 220 R=1,2**FLAG(6)
					CGN(1,P) = CG(1,N)+((R-1)*NCW)
					CGN(2,P) = CGN(1,P) + NCW
					CGN(3,P) = CG(4,N) + ((Q-1)*NCH)
      			CGN(4,P) = CGN(3,P) + NCH 
					CT(P)	= CT(N)
				DO 111 L = 1, NSP_T
					DO 112 LL = 1, NSP_T
						CARC(1,P,L,LL) = SQRT(BOLTZ*SPM(2,L,LL)/SPM(5,L,LL))*SPM(1,L,LL)
						CARC(2,P,L,LL) = RF(rank)
112					CONTINUE
111 			CONTINUE
				P=P+1
220 CONTINUE
110			CONTINUE
			P=P-1
		ELSE
			CGN(1,P)=CG(1,N)
			CGN(2,P)=CG(2,N)
			CGN(3,P)=CG(4,N)
			CGN(4,P)=CG(5,N)
			CT(P)	= CT(N)
			DO 101 L = 1, NSP_T
					DO 102 LL = 1, NSP_T
						CARC(1,P,L,LL) = CARC(1,N,L,LL)
						CARC(2,P,L,LL) = CARC(2,N,L,LL)
102					CONTINUE
101 			CONTINUE
		END IF
100 CONTINUE
NC_D = P
INC_C = NC_OLD-NC_D
NSC_D = NC_D*NSCX*NSCY

	DO 200 N = 1,NC_D
		CG(1,N) = CGN(1,N)
		CG(2,N) = CGN(2,N)
		CG(3,N) = CG(2,N)-CG(1,N)
		CG(4,N) = CGN(3,N)
		CG(5,N) = CGN(4,N)
		CG(6,N) = CG(5,N)-CG(4,N)
200 CONTINUE

CALL CELLV

DO 300 N = 1,NC_D
		DO 310 MY = 1,NSCY
			DO 311 MX = 1,NSCX
				M=(N-1)*NSCX*NSCY+(MY-1)*NSCX+MX
				ISC(M)=N
311      CONTINUE
310   CONTINUE
300 CONTINUE

	DO 400 P=1,NC_OLD
		R=0
		DO 450 Q=1,P-1
			IF(CAR(Q).EQ.1)R=R+1
450	CONTINUE
		CAB(P)=R*4**FLAG(6)+(P-R)
400 CONTINUE

	DO 500 I=1,NM
		CALL CELLFINDER(I,PP(1,I),PP(2,I))
500 CONTINUE
END SUBROUTINE