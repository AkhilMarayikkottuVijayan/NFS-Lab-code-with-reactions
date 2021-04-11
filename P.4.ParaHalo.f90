SUBROUTINE HALO
USE VARIABLES
USE MPI
INTEGER :: P,Q,R,Pos
	IF(rank.NE.0) THEN
		Sdata = SC(1)
		Ssize = 1
		destin = rank-1
		tag = 1
		CALL MPI_SEND(Sdata,Ssize,MPI_INTEGER,destin,tag,MPI_COMM_WORLD,ierr) 
	END IF
	IF (rank.NE.nprocs-1) THEN
		Rsize = 1
		tag = 1
		Sour = rank+1
		CALL MPI_RECV(Rdata,Rsize,MPI_INTEGER,sour,tag,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr) 
		RC(2)=Rdata
	END IF
	IF (rank.NE.nprocs-1) THEN
		Sdata = SC(2)
		Ssize = 1
		destin = rank+1
		tag = 2
		CALL MPI_SEND(Sdata,Ssize,MPI_INTEGER,destin,tag,MPI_COMM_WORLD,ierr) 
	END IF
	
	IF (rank.NE.0)	THEN
		Rsize = 1
		tag = 2
		Sour = rank-1
		CALL MPI_RECV(Rdata,Rsize,MPI_INTEGER,sour,tag,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr) 
		RC(1)=Rdata
	END IF

CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

DO P=1,10
	IF(rank.NE.0) THEN
		Pos = 1
		DO Q=1,SC(Pos)
			Sdata1 = PACKS(Pos,Q,P)
			Ssize = 1
			destin = rank-1
			tag = 3
			CALL MPI_SEND(Sdata1,Ssize,MPI_DOUBLE_PRECISION,destin,tag,MPI_COMM_WORLD,ierr) 
		END DO
	END IF
	
	IF (rank.NE.nprocs-1) THEN
		Pos = 2
		DO R=1,RC(Pos)
			Rsize = 1
			tag = 3
			Sour = rank+1
			CALL MPI_RECV(Rdata1,Rsize,MPI_DOUBLE_PRECISION,sour,tag,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr) 
			PACKR(Pos,R,P)=Rdata1
		END DO
	END IF
	
	IF (rank.NE.nprocs-1) THEN
		Pos = 2
		DO Q=1,SC(Pos)
			Sdata1 = PACKS(Pos,Q,P)
			Ssize = 1
			destin = rank+1
			tag = 4
			CALL MPI_SEND(Sdata1,Ssize,MPI_DOUBLE_PRECISION,destin,tag,MPI_COMM_WORLD,ierr) 
		END DO
	END IF
	
	IF (rank.NE.0) THEN
		Pos = 1
		DO R=1,RC(Pos)
			Rsize = 1
			tag = 4
			Sour = rank-1
			CALL MPI_RECV(Rdata1,Rsize,MPI_DOUBLE_PRECISION,sour,tag,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr) 
			PACKR(Pos,R,P)=Rdata1
		END DO
	END IF
		
END DO

CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

		DO 150 I = 1, RC(2)
			NM = NM + 1
			MSTAT(NM) = 0
			DO 120 P = 1, 10
				PP(P,NM) = PACKR(2,I,P)
120		CONTINUE
			CALL CELLFINDER(NM,PP(1,NM),PP(2,NM))
150	CONTINUE
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
DO 250 I = 1, RC(1)
			NM = NM + 1
			MSTAT(NM) = 0
			DO 220 P = 1, 10
				PP(P,NM) = PACKR(1,I,P)
220		CONTINUE
			CALL CELLFINDER(NM,PP(1,NM),PP(2,NM))
250	CONTINUE
RETURN
END SUBROUTINE
