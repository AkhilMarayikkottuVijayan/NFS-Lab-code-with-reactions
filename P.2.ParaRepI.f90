SUBROUTINE PARAREPI
USE VARIABLES
USE MPI

 DOUBLE PRECISION :: A_Temp(13)
 INTEGER			  :: R,I_Temp(10)
 CHARACTER(10) :: FPre
 CHARACTER(3) :: FSuf
 
 DOUBLE PRECISION :: Xtemp,Ytemp
 !CHARACTER() :: Fname 
 
 IF (rank.NE.0)THEN
	Sdata = NM
	Ssize = 1
	destin = 0
	tag = 1
	CALL MPI_SEND(Sdata,Ssize,MPI_INTEGER,destin,tag,MPI_COMM_WORLD,ierr) 
	
	Sdata = NC_D
	Ssize = 1
	destin = 0
	tag = 2
	CALL MPI_SEND(Sdata,Ssize,MPI_INTEGER,destin,tag,MPI_COMM_WORLD,ierr) 
 END IF

 IF (rank.EQ.0) THEN
 	SD_N(1) = NM
 	SD_C(1) = NC_D
	DO R= 1, nprocs-1
		Rsize = 1
		tag = 1
		Sour = R
		CALL MPI_RECV(Rdata,Rsize,MPI_INTEGER,sour,tag,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr) 
		SD_N(R+1)=Rdata
		
		Rsize = 1
		tag = 2
		Sour = R
		CALL MPI_RECV(Rdata,Rsize,MPI_INTEGER,sour,tag,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr) 
		SD_C(R+1)=Rdata
	END DO
 END IF
 

 CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
 
 IF (rank.EQ.0) THEN
	
	OPEN (20,FILE='2.3.Grid Definition.dat',FORM='FORMATTED')
		WRITE (20,*)'TITLE = "Grid map"'
		WRITE (20,*)'VARIABLES = "Xmin","Xmax","DelX","Ymin","Ymax","DelY","Vol","Tem","CID","SCID"'
		DO R=0,nprocs-1
			FPre="z-2.3.Grid" 
			WRITE(FSuf,'(I3.3)')R
			OPEN (R+200,FILE=Fpre//Fsuf//'.dat',FORM='FORMATTED')
				DO J=1,SD_C(R+1)	
					READ(R+200,*)A_Temp(1:8), I_Temp(1:2)
					WRITE(20,*) A_Temp(1:8), I_Temp(1:2)
				END DO
			CLOSE(R+200,STATUS="DELETE")	
		END DO
	CLOSE(20)
 END IF


END SUBROUTINE
