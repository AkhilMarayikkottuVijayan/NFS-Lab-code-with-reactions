SUBROUTINE SPECIES
USE VARIABLES
 INTEGER :: Q  
!---------------------Reading Species Properties----------!


	OPEN(1,FILE="1.2.Species Database.txt", FORM="FORMATTED")
		READ(1,*)
		READ(1,*) NSP_T 
		ALLOCATE(SP(14,1:NSP_T),IG(2,1:NSP_T),SPM(1:14,1:NSP_T,1:NSP_T))
		ALLOCATE(SP_SY(1:NSP_T),SP_CON(1:NSP_T),MAXV_DOF(1:NSP_T))
		ALLOCATE(SP_NA(1:NSP_T),SP_FR(1:NSP_T),SP_EX(1:NSP_T))
		ALLOCATE(LB_MODEL(2))
		DO 100 L=1,NSP_T
			SP_EX(L) = 0
			SP_FR(L) = 0
			READ(1,*)
			READ(1,*)
			READ(1,*) SP_SY(L)
			READ(1,*) SP_NA(L)
			READ(1,*) SP_CON(L)
			READ(1,*) SP(1,L)
			READ(1,*) SP(2,L)
			READ(1,*) SP(3,L)
			READ(1,*) SP(4,L)
			READ(1,*) SP(5,L)
			READ(1,*) SP(6,L)
			READ(1,*) SP(7,L)
			READ(1,*) SP(8,L)
			READ(1,*) SP(9,L)
			READ(1,*) SP(10,L),SP(11,L),SP(12,L)
			READ(1,*) SP(13,L),SP(14,L)
			IF(SP_CON(L).EQ."LIN")MAXV_DOF(L) = 3*SP_NA(L)-5
			IF(SP_CON(L).EQ."NON")MAXV_DOF(L) = 3*SP_NA(L)-6
100 	CONTINUE
	CLOSE(1)
!----------------------------------------------------------------!
!---------------------Reading Cross Species Properties----------!
!----------------------------------------------------------------!

OPEN(2, FILE="1.3.CrossSpecies Database.txt",FORM="FORMATTED")
	DO 400 L = 1, NSP_T
		DO 410 LL = 1, NSP_T
			DO 420 Q=1,14
				SPM(Q,L,LL)=(SP(Q,L)+SP(Q,LL))/2
				IF(Q.EQ.1)SPM(1,L,LL) = PI*((SP(1,L)+SP(1,LL))/2)**2
				IF(Q.EQ.5)SPM(5,L,LL) = SP(5,L)*SP(5,LL)/(SP(5,L)+SP(5,LL))
420		CONTINUE
410	CONTINUE
400 CONTINUE
 CLOSE(2)


DO L = 1, NSP_T
		DO  LL = 1, NSP_T
           IF(L.NE.LL) THEN
  SPM(13,L,LL)=1800
  SPM(14,L,LL)= 73.5
 
 END IF
 END DO
 END DO


IF(rank.eq.0)WRITE(*,*)"PREPROSSING-SPECIES.......... SUCCESSFUL"
END SUBROUTINE

