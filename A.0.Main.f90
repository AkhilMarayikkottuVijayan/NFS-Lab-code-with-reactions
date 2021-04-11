    PROGRAM DSMC
	USE MPI
	USE VARIABLES
	
	ALLOCATE(NTS(2),Check(4),FLAG(9))

	CALL PARAINIT
	OPEN(111,FILE="1gnosis.dat",FORM="FORMATTED")
	CALL SPECIES
	CALL READ
        CALL EXCHANGE_INIT
        CALL REACTION

	CALL PREPROS
      	NSIM = 5000000
	ALLOCATE(PP(10,1:NSIM),MSTAT(1:NSIM),MJETS(1:NSIM),IP(1:NSIM),IR(1:NSIM))
	ALLOCATE(PACKS(1:2,1:NSIM,1:10))
	ALLOCATE(PACKR(1:2,1:NSIM,1:10))
        ALLOCATE(DISS(1:NSIM),RPAIR(1:NSIM,2))
	CALL REPORT
	CALL SAMPINIT
	CALL CHECKS
	
	CALL PARAREPI
	
	OPEN(13,FILE="3.1.Particle Hist.dat",FORM="FORMATTED")
	OPEN(14,FILE="2.9.Diagnosis.dat",FORM="FORMATTED")
	DISS = 0	

        NPR = 0
100 	NPR=NPR+1
	
	CALL SAMPLE
	CALL TIMEAVG
	CALL MOVE
	
	CALL HALO

	IF(NPR.EQ.(NTS(1)-5000) .AND.(FLAG(6).GT.0)) CALL ADAPTIVE

	CALL INDEX

        IF(FLAG(8).EQ.2 .AND. NPR.GT.(NTS(1)-1000).AND.NPR.LT.NTS(1)) CALL DETECTEQFCELL

        IF(FLAG(8).EQ.3 .AND. NPR.GT.(NTS(1)-5000) .AND. NPR.LT.NTS(1)) THEN
                DELU = 100.0
                CALL VELCOLLECTION
                IF(NPR.EQ.(NTS(1)-1)) CALL KSTEST
        END IF
        DISS = 0
IF(rank.EQ.0) WRITE(*,*) NPR 
	CALL COLLISION
       
        IF(FLAG(9).EQ.1)THEN
        CALL ADD_REMOVE
        CALL RECOMBINE
        END IF

	IF(MOD(NPR,10) .EQ. 0) CALL DIAGNOSTICS

	IF(NPR.GT.NTS(1))THEN
		IF((Check(1).EQ.1).AND.(FLAG(7).EQ.0))CALL SAMPVDF
	END IF

	IF(NPR.LT.NTS(2))GO TO 100		
	
	CALL OUTPUT
	
	CALL PARAREPF
CLOSE(13)
CLOSE(14)
close(111)
CALL MPI_FINALIZE(ierr)
STOP
END PROGRAM
