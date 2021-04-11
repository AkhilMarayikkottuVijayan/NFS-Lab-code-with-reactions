SUBROUTINE REPORT	
USE VARIABLES
  CHARACTER(8)::BCOND,LOCAT
  
 OPEN(23,FILE='2.1.Report.dat',FORM='FORMATTED')
 	WRITE(23,*)"-----------------DOMAIN PARAMETERS------------------"
	WRITE(23,*)
	WRITE(23,*)"Domain Length (in meters)			:- ",W_D
	WRITE(23,*)"Domain Height (in meters)			:- ",H_D
	WRITE(23,*)"Domain Height (in meters)			:- ",D_D
	WRITE(23,*)"Domain Volume (in meters**3)		:- ",VOL_D
	WRITE(23,*)"No. of cells				:- ",NC_D
	WRITE(23,*)"No. of cells in X direction		        :- ",NCX_D
	WRITE(23,*)"No. of cells in Y direction		        :- ",NCY_D
	WRITE(23,*)"No. of Subcells				:- ",NSC_D
	WRITE(23,*)"No. of Subcells/Cell in X			:- ",NSCX
	WRITE(23,*)"No. of Subcells/Cell in Y			:- ",NSCY
	WRITE(23,*)
	WRITE(23,*)"-----------------DOMAIN CONDITION------------------"
	WRITE(23,*)
	WRITE(23,*)"Number density of the domain 		:- ",FND_D
	WRITE(23,*)"Number of simulated molecules in domain	:- ",FND_D/FNUM
	WRITE(23,*)"Temperature of the domain			:- ",TMP_D
	WRITE(23,*)"Pressure  of the domain			:- ",FND_D*TMP_D*BOLTZ
	WRITE(23,*)
	WRITE(23,*)"-----------------BOUNDARY CONDITION------------------"
	WRITE(23,*)
	 DO i=1,4
	 	IF(BC(i).EQ.1) BCOND="PERIODIC"
	 	IF(BC(i).EQ.2) BCOND="SYMMETRY"
	 	IF(BC(i).EQ.3) BCOND="VACCUM"	
	 	IF(BC(i).EQ.4) BCOND="AXISYMMETRY"	
	 	IF(i.EQ.1) LOCAT="LEFT"
	 	IF(i.EQ.2) LOCAT="RIGHT"
	 	IF(i.EQ.3) LOCAT="BOTTOM"
	 	IF(i.EQ.4) LOCAT="TOP"
	 	WRITE(23,*) LOCAT," boundary is ",BCOND
	 END DO
	WRITE(23,*)
	WRITE(23,*)"-----------------BODY DEFINITION------------------"
	WRITE(23,*)
	IF(FLAG(2).EQ.1) THEN
		WRITE(23,*) "Body present in the domain is 	:- ",BODYNAME
		WRITE(23,*) "Number of bodies in the domain	:- ",NB
		WRITE(23,*) "Body`s range of influence is(min)	:- ",OVER(1),OVER(3)
		WRITE(23,*) "Body`s range of influence is(max)	:- ",OVER(3),OVER(4)
	ELSE
		WRITE(23,*) "No body in flow"
	END IF
	WRITE(23,*)
	
		WRITE(23,*)"-----------------JET PARAMETERS------------------"
	IF(NJET.NE.0) THEN
	 	WRITE(23,*)
	 	DO j=1,NJET
		 	WRITE(23,*)'Jet number 			:-	',J
		 	WRITE(23,*)'Jet name 			:-	',DESC_J(J)
			WRITE(23,*)'Initial jet coord		:-	',JET(1,J),JET(3,J)
			WRITE(23,*)'Final jet coord		:-	',JET(2,J),JET(4,J)
		 	WRITE(23,*)'Jet temp (K)		:-	',TMP_J(J)
		 	WRITE(23,*)'Jet angle (deg)		:-	',JET(6,J)*180/PI
		 	WRITE(23,*)'Jet speed (m/s)		:-	',VP_J(1,J)
		 	WRITE(23,*)'Jet vel X (m/s)		:-	',VP_J(2,J)
		 	WRITE(23,*)'Jet vel Y (m/s)		:-	',VP_J(3,J)
		 	WRITE(23,*)'Jet profile			:-	',PROF_J(J)
		 	
		 	IF (PROF_J(j).NE.'CONSTANT')THEN
		 		WRITE(23,*)'Jet peak (m)		:-	',VP_J(4,J)
		 		WRITE(23,*)'Jet top velocity (m/s)	:-	',VP_J(5,J)
		 		WRITE(23,*)'Jet bottom velocity (m/s)	:-	',VP_J(6,J)
		 	END IF	
		 	DO L=1,NSP_T
		 		IF (SP_EX(L).EQ.1)THEN
		 			WRITE(23,*)"No. of molecules of ",SP_SY(L)," entering 	:- ",NINT(ENT_J(J,L))
		 		END IF
		 	END DO
	 	END DO
	ELSE
		 	WRITE(23,*)"No molecules enter the domian via jets"
	END IF
	WRITE(23,*)
		WRITE(23,*)"-----------------SPECIES PARAMETERS------------------"
	WRITE(23,*)
	WRITE(23,*)"DISTINCT SPECIES IN SIMULATION ",NSP_T
	DO L=1,NSP_T
		IF (SP_EX(L).EQ.1) THEN
			WRITE(23,*)
			WRITE(23,*)"SPECIES NAME		:- ",SP_SY(L)
			WRITE(23,*)"DIAMETER			:- ",SP(1,L)
			WRITE(23,*)"REFERENCE TEMP		:- ",SP(2,L)
			WRITE(23,*)"POWER LAW INDEX		:- ",SP(3,L)
			WRITE(23,*)"VSS SCATTERING PARAM	:- ",SP(4,L)
			WRITE(23,*)"MASS			:- ",SP(5,L)
			WRITE(23,*)"GAMMA			:- ",SP(6,L)
			WRITE(23,*)"No. ROTATIONAL DOF		:- ",SP(7,L)
			WRITE(23,*)"No. VIBRATIONAL DOF		:- ",SP(8,L)
			WRITE(23,*)"CHAR. VIBRATIONAL TEMP	:- ",SP(9,L)
		END IF
	END DO
	WRITE(23,*)
	WRITE(23,*)"-----------------RUN PARAMETERS------------------"
	WRITE(23,*)
	WRITE(23,*)"Time step (secs)				:- ",DTM
	WRITE(23,*)"Total simulation time (secs)		:- ",NTS(2)*DTM
	WRITE(23,*)"Tentative steps till steady state 	        :- ",NTS(1)
	WRITE(23,*)"Number of productive steps 			:- ",(NTS(2)-NTS(1))
	WRITE(23,*) 
 CLOSE(23)
IF(rank.EQ.0) THEN
	WRITE(*,*)
	WRITE(*,*)"REPORT GENERATION ........... SUCCESSFUL"
	WRITE(*,*)
END IF
END SUBROUTINE
