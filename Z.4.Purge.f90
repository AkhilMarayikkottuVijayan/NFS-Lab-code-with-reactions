SUBROUTINE PURGE
USE VARIABLES
INTEGER ::Ori
Ori=NM
N=0
95 N=N+1
IF(N.LE.NM)THEN
	IF((MSTAT(N).EQ.1).OR.(MSTAT(N).EQ.4)) THEN
	 	DO 105 M=1,10
	 		PP(M,N) = PP(M,NM)
105 	CONTINUE
		MSTAT(N)=MSTAT(NM)
		IP(N)=IP(NM)
		NM = NM-1
		N  = N-1
	GO TO 95
	END IF
	GO TO 95
END IF

N=0
195 N=N+1
IF(N.LE.NM)THEN
	IF(MSTAT(N).EQ.3) THEN
	 	DO 115 M=1,10
	 		PP(M,N) = PP(M,NM)
115 	CONTINUE
		MSTAT(N)=MSTAT(NM)
		IP(N)=IP(NM)
		NM = NM-1
		N  = N-1
	GO TO 95
	END IF
	GO TO 195
END IF



DO I= 1,NM
	MSTAT(I)=0
END DO
Rmv=Ori-NM
END SUBROUTINE
