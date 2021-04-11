SUBROUTINE TIMEAVG
USE VARIABLES
 DOUBLE PRECISION :: A,UU
 DOUBLE PRECISION :: TTEM,RTEM,VTEM,ToTEM
 DOUBLE PRECISION :: TDOF,RDOF,VDOF,KE(1:NSP_T)
 INTEGER :: Q,L,N
 DO 90 N=1,NC_D	
	 DO 80 Q=1,13
		CSN(Q,N) = 0
80  CONTINUE
90 CONTINUE
  
 DO 100 N=1,NC_D	
	DO 110 L=1,NSP_T 
		IF(SP_EX(L).EQ.1)THEN
			IF (SP(8,L).GT.0) THEN
				IF (CS(12,N,L).GT.0.0) THEN
					TVIB(N,L) = SP(9,L)/LOG(1+(BOLTZ*SP(9,L)*CS(1,N,L)/CS(12,N,L)))
					SVIB(N,L)=2.*CS(12,N,L)/(CS(1,N,L)*BOLTZ*TVIB(N,L))
				ELSE
					TVIB(N,L)=1.E-6
					SVIB(N,L)=0.
				END IF
			END IF					
		END IF
	
110	CONTINUE
100 CONTINUE

   DO 200 N=1,NC_D	
	DO 210 L=1,NSP_T 
		IF(SP_EX(L).EQ.1)THEN
			CSN(1,N) = CSN(1,N)+CS(1,N,L)      !!!!!!!!cell sum of no. of molecules across species!!!!!!!!!
			CSN(2,N) = CSN(2,N)+(SP(5,L)*CS(1,N,L))         !!!!!! cell sum of mass of mols across species!!!!!!!!!!
			DO 211 K=3,5
				CSN(K,N)   = CSN(K,N)+SP(5,L)*CS(K-1,N,L)    !!!!cell sum of momentum!!!!
				CSN(K+3,N) = CSN(K+3,N)+SP(5,L)*CS(K+2,N,L)           !!!!!!!cell sum of KE!!
211  		CONTINUE
			CSN(9,N)  = CSN(9,N)+(CSN(6,N)+CSN(7,N)+CSN(8,N))		!!!!!cell sum of total KE!!!!!!!!	
			CSN(10,N) = CSN(10,N)+CS(8,N,L)                                !!!!!!!cell sum of RE!!!!!
			CSN(11,N) = CSN(11,N)+SP(7,L)*CS(1,N,L)                             !!!!!sum of RDOF!
			CSN(12,N) = CSN(12,N)+TVIB(N,L)*SVIB(N,L)*CS(1,N,L)                  !!!!!!cell sum of VE!!!!
			CSN(13,N) = CSN(13,N)+SVIB(N,L)*CS(1,N,L)		                     !!!!!!CELL SUM OF VDOF!!!!
                  
		 END IF
210 	CONTINUE
200 CONTINUE


 DO 300 N=1,NC_D
		A=FNUM/(CC(N)*NSMP)
		IF(CC(N).EQ.0)A = 0
	 	DO 310 K=1,3
			VELOP(K) = CSN(K+2,N)/CSN(2,N)	
		310 	CONTINUE		
		UU	  = VELOP(1)**2+VELOP(2)**2+VELOP(3)**2
TTEM = 0.0D0
      DO 2000 L=1,NSP_T	
	   	            KE(L) = (CS(5,N,L)+CS(6,N,L)+CS(7,N,L))/CS(1,N,L)
		     
                       KE(L)=SP(5,L)*KE(L)-SP(5,L)*UU
			
	!TTEM = ((CS(1,N,1)*KE1)+(CS(1,N,2)*KE2))/(3*BOLTZ*(CS(1,N,1)+CS(1,N,2)))
           TTEM = TTEM + (CS(1,N,L)*KE(L))/(3*BOLTZ*CSN(1,N))

2000 CONTINUE	
               TDOF = 3
			
		IF(CSN(11,N).NE.0) THEN
			RTEM = (2/BOLTZ)*CSN(10,N)/CSN(11,N)
			RDOF = CSN(11,N)/CSN(1,N)
		ELSE
			RTEM = 0
			RDOF = 0
		END IF
	
		IF(CSN(13,N).NE.0) THEN
			VTEM = CSN(12,N)/CSN(13,N)
			VDOF = CSN(13,N)/CSN(1,N)
		ELSE
			VTEM = 0
			VDOF = 0
		END IF
		TRNS_T(N)=TTEM 
	 	CT(N)= ((TDOF*TTEM)+(RDOF*RTEM)+(VDOF*VTEM))/(TDOF+RDOF+VDOF)
300 CONTINUE

	RETURN
END SUBROUTINE
