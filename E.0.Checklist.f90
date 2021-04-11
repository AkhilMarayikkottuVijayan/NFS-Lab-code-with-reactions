SUBROUTINE CHECKS
USE VARIABLES
USE MPI
INTEGER :: MC
 CHARACTER(10) :: FPre
 CHARACTER(3) :: FSuf
 !CHARACTER() :: Fname 
IF(rank.eq.0)WRITE(*,*)"					CHECKLIST				"


 IF(Check(2).EQ.1) THEN
 
 	FPre = 'z-2.2.Init'
 	WRITE(FSuf,'(I3.3)')rank
	 OPEN(rank+1,FILE=FPre//FSuf//'.dat',FORM='FORMATTED')
		DO I=1,NM
			WRITE(rank+1,*)PP(1:2,I),I
		END DO
	 CLOSE(rank+1)
	 
	 
	 IF(rank.eq.0)WRITE(*,*)"INITIAL DISTRIBUTION----------DONE"
 END IF
 
 IF(Check(3).EQ.1) THEN
 
 	FPre = 'z-2.3.Grid'
 	WRITE(FSuf,'(I3.3)')rank
	 OPEN(rank+10,FILE=FPre//FSuf//'.dat',FORM='FORMATTED')
	 	DO M=1,NSC_D
			MC=ISC(M)
			WRITE(rank+10,*)CG(1:6,MC),CC(MC),CT(MC),M,MC
	 	END DO
	 CLOSE(rank+10)
WRITE(*,*)"GRID INITIALIZATION ",rank,"-----------DONE"
END IF

CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

 IF(Check(4).EQ.1) THEN
	 OPEN(146,FILE='2.4.Body Definition.dat',FORM='FORMATTED')
	 	DO K=1,TNS
			WRITE(146,*)K,BD(K),SURF(1:4,K)
	 	END DO
	 CLOSE(146)
IF(rank.eq.0)WRITE(*,*)"BODY INITIALIZATION-----------DONE"
END IF

OPEN(145,FILE='2.5.Warnings.dat',FORM='FORMATTED')
	DO I=1,4
		IF(BC(I).EQ.4)FLAG(3)=FLAG(3)+1
	END DO
	IF (FLAG(3).GT.1) WRITE(145,*)" Warning :: Only one surface can be axisymmetric"
 CLOSE(145)
 
END SUBROUTINE

