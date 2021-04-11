SUBROUTINE	PBOUNDS(I,X,Y)
	USE VARIABLES
	INTEGER 				:: I,Q,Pos,II
	DOUBLE PRECISION  :: X,Y
	IF(MSTAT(I).NE.1)THEN
		IF(Y.LT.CB(3))THEN
			CountTr=CountTr+1
			MSTAT(I)=4
			Pos = 1
			SC(Pos)=SC(Pos)+1
			DO Q = 1, 10
				PACKS(Pos,SC(Pos),Q) = PP(Q,I)
			END DO
		END IF
		IF(Y.GE.CB(4))THEN
			CountTr=CountTr+1
			MSTAT(I)=4
			Pos = 2
			SC(Pos)=SC(Pos)+1
			DO Q = 1, 10
				PACKS(Pos,SC(Pos),Q) = PP(Q,I)
			END DO
		END IF
	END IF
RETURN
END SUBROUTINE
