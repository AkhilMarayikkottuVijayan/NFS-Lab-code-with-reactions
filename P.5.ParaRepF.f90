SUBROUTINE PARAREPF
USE VARIABLES
USE MPI

 DOUBLE PRECISION :: A_Temp(15)
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
	OPEN (10,FILE='3.2.Final Position.dat',FORM='FORMATTED')
		WRITE (10,*)'TITLE = "Final Position"'
		WRITE (10,*)'VARIABLES = "X (m)","Y (m)","ID"'
		DO R=0,nprocs-1
		   FPre = 'z-3.2.Fina'
			WRITE(FSuf,'(I3.3)')R
			OPEN (R+100,FILE=Fpre//Fsuf//'.dat',FORM='FORMATTED')
				DO J=1,SD_N(R+1)	
					READ(R+100,*)A_temp(1),A_temp(2),I_temp(1)
					WRITE(10,*)A_temp(1),A_temp(2),I_temp(1)
				END DO
			CLOSE(R+100,STATUS="DELETE")
		END DO
	CLOSE(10) 

	OPEN (20,FILE='3.4.Flow Properties.dat',FORM='FORMATTED')


		WRITE (20,*)'TITLE = "Flow Properties"'
		WRITE (20,*)'VARIABLES = "X (m)","Y (m)","U","V","W","Density","NumDen","Pressure","Temp Tran","Temp Rot",&
                "Temp Vib","Temp Overall","Mach No.","Eq-breakdown Param","Solver Domain"'
		IF(FLAG(6).EQ.0) WRITE (20,*)'ZONE  T="Results at ',NPR*DTM,' s", I = ',NCX_OA,' J = ',NCY_OA,' F = POINT'
		DO R=0,nprocs-1
			FPre = 'z-3.4.Flow' 
			WRITE(FSuf,'(I3.3)')R
			OPEN (R+200,FILE=Fpre//Fsuf//'.dat',FORM='FORMATTED')
				DO J=1,SD_C(R+1)	
					READ(R+200,*)A_Temp(1:15)
					WRITE(20,*) A_Temp(1:15)
				END DO
			CLOSE(R+200,STATUS="DELETE")	
		END DO
	CLOSE(20)


DO L=1, NSP_T
IF(SP_EX(L).EQ.1)THEN
	OPEN (L+102,FILE='3.6.Flow Properties'//SP_SY(L)//'.dat',FORM='FORMATTED')
			WRITE (L+102,*)'TITLE = "Flow Prop Species"',SP_SY(L)
			WRITE (L+102,*)'VARIABLES = "X (m)","Y (m)","U","V","W","Density","NumDen","Pressure","Temp T",&
                        "Temp R","Temp V","Temp Overall","Mach No."'
			WRITE (L+102,*)'ZONE  T="Result at ',NPR*DTM,'", I = ',NCX_OA,' J = ',NCY_OA,' F = POINT'		
DO R=0,nprocs-1
			FPre = 'z-3.6.Flsp' 
			WRITE(FSuf,'(I3.3)')R
			OPEN (R+201,FILE=Fpre//Fsuf//SP_SY(L)//'.dat',FORM='FORMATTED')
				DO J=1,SD_C(R+1)	
					READ(R+201,*)A_Temp(1:13)
					WRITE(L+102,*) A_Temp(1:13)
				END DO
			CLOSE(R+201,STATUS="DELETE")	
		END DO
	CLOSE(L+102)
END IF
END DO
	
END IF
END SUBROUTINE
