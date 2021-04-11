SUBROUTINE MACRO
 USE VARIABLES
 INTEGER 		  :: Q
 DOUBLE PRECISION :: A,UU,TRI
 DOUBLE PRECISION :: NDEN,DEN,PRES
 DOUBLE PRECISION :: Kn
 DOUBLE PRECISION :: TTEM,RTEM,VTEM,VTEMS(1:NSP_T),ToTEM,TVDOF
 DOUBLE PRECISION :: TDOF,RDOF,VDOF,MACH,KE(1:NSP_T),VDOFS(1:NSP_T)
 !!DOUBLE PRECISION :: CP_EF,CV_EF,TOTD,GAMM,M,GASC
    

 CHARACTER(10) :: FPre
 CHARACTER(3) :: FSuf
	DO 100 N=1,NC_D	
		DO 110 Q=1,14
			CSN(Q,N) = 0
110 	CONTINUE
100 CONTINUE
DO L=1,NSP_T
	C_V(L)=0
	C_P(L)=0
END DO
TOTD=0
 CP_EF =0
 CV_EF=0
 
DO L=1,NSP_T
	IF(SP_EX(L).EQ.1)THEN
	GASC =8.31446/(SP(5,L)*6.022E+23) 
	
       C_V(L)=GASC/(SP(6,L)-1)				!!ADD GAS CONSTANT
	C_P(L)=C_V(L)+GASC
     
	M=SP_FR(L)*SP(5,L)*6.022E+23*FND_D*(OB(2)-OB(1))*(OB(4)-OB(3))
	CV_EF=CV_EF+C_V(L)*SP_FR(L)*M
	CP_EF = CP_EF + C_P(L)*SP_FR(L)*M
       
	TOTD = TOTD + SP_FR(L)*M
     
	END IF
END DO
	CV_EF=CV_EF/TOTD
	CP_EF = CP_EF/TOTD 	
	R_STREAM = CP_EF - CV_EF
	GAMM = CP_EF/CV_EF
	WRITE(111,*)"kj",CP_EF,CV_EF,TOTD 
	!GAMM  = 1.667
	!R_STREAM = 208.0
	KE1=0
      KE2=0
BULK1=0
BULK2=0

	DO 200 N=1,NC_D	
		DO 210 L=1,NSP_T 
IF(SP_EX(L).EQ.1)THEN
			CSN(1,N) = CSN(1,N)+CS(1,N,L)              !!!!!!!!sum of species mole
			CSN(2,N) = CSN(2,N)+(SP(5,L)*CS(1,N,L))        !!!!!sum of species mass
				DO 211 K=3,5
				CSN(K,N)   = CSN(K,N)+SP(5,L)*CS(K-1,N,L)       !!!!!!!sum of momentum
				CSN(K+3,N) = CSN(K+3,N)+SP(5,L)*CS(K+2,N,L)          !!!!!!sum of kinetic energy
                          

211    		CONTINUE
			CSN(9,N)  = CSN(9,N)+(CSN(6,N)+CSN(7,N)+CSN(8,N))         !!!!!!!!cell sum of KE			
			CSN(10,N) = CSN(10,N)+CS(8,N,L)                             !!!!!!!!!!CELL SUM OF RE
			CSN(11,N) = CSN(11,N)+SP(7,L)*CS(1,N,L)                        !!!!CELL SUM OF RDOF
			CSN(13,N) = CSN(13,N)+LOG(CS(10,N,L)/CS(11,N,L))                 !!!!!CELL SUM OF VDOF
			CSN(14,N) = CSN(14,N)+CS(12,N,L)
	
END IF
210 	CONTINUE
200 CONTINUE
	WRITE(111,*)rank,R_STREAM,GAMM 
!IF(rank.eq.0)Write(14,*)"kjw",R_STREAM,CP_EF,TOTD
FPre = 'z-3.4.Flow'
WRITE(FSuf,'(I3.3)')rank

OPEN (Rank+11,FILE=FPre//FSuf//'.dat',FORM='FORMATTED',ACCESS="APPEND")


	DO 800 N=1,NC_D

                                                   

			A=FNUM/(CC(N)*NSMP)
			IF(CC(N).EQ.0)A = 0
			DO 810 K=1,3
				VELOP(K) = CSN(K+2,N)/CSN(2,N)
                 	
810		CONTINUE		
	  
			NDEN = CSN(1,N)*A
			DEN  = NDEN*CSN(2,N)/CSN(1,N)
			UU	  = VELOP(1)**2+VELOP(2)**2+VELOP(3)**2
			TTEM = 0.0D0
DO 2000 L=1,NSP_T	
	   	            KE(L) = (CS(5,N,L)+CS(6,N,L)+CS(7,N,L))/CS(1,N,L)
		     
                       KE(L)=SP(5,L)*KE(L)-SP(5,L)*UU
	
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
			VTEM = 0.0D0
			TVDOF = 0.0D0
			IF(CSN(14,N).NE.0) THEN
				
			DO 2001 L = 1,NSP_T
				IF(SP(9,L).GT.0.AND.CS(12,N,L).GT.0.0 .AND. CS(1,N,L) .GE.1.0) THEN
				VTEMS(L) = SP(9,L)/LOG(1+(BOLTZ*SP(9,L)*CS(1,N,L)/CS(12,N,L)))
                                ELSE
                                VTEMS(L) = 0
                                END IF
                               ! IF(SP(8,L).GT.0 .AND. CS(1,N,L) .GE.1.0) THEN
                                IF(VTEMS(L).GT.0.0)THEN
				VDOFS(L) = (2*CS(12,N,L))/(BOLTZ*VTEMS(L)*CS(1,N,L))
                      	VTEM = VTEM + VDOFS(L)*VTEMS(L)
				TVDOF = TVDOF+VDOFS(L)
				END IF
			2001 CONTINUE
                                IF(VTEM.GT.0.AND.TVDOF.GT.0)THEN
		        		VTEM = VTEM/TVDOF
			        	VDOF = TVDOF/CSN(1,N)
                                ELSE
                                        VTEM = 0.00
                                        VDOF = 0.00
                                END IF
  
			ELSE
				VTEM = 0
				VDOF = 0
			END IF
        IF(VTEM.NE.VTEM)WRITE(781576,*) VDOFS(:),VTEMS(:),TVDOF,VTEM 
			!!WRITE(111,*)"INSIDE",rank,R_STREAM,GAMM 
			ToTEM= ((TDOF*TTEM)+(RDOF*RTEM)+(VDOF*VTEM))/(TDOF+RDOF+VDOF)
			PRES = DEN*R_STREAM*ToTEM
			!WRITE(111,*)"INSIDE",rank,R_STREAM,GAMM 
			MACH=SQRT(VELOP(1)**2+VELOP(2)**2+VELOP(3)**2)/SQRT(GAMM*R_STREAM*ToTEM)	!!FOR SINGLE SPECIES				
			IF(ToTEM .LE.1E-2)MACH=0
		
		XC = 0.5*(CG(1,N)+CG(2,N))
		YC = 0.5*(CG(4,N)+CG(5,N))

		WRITE(Rank+11,*)XC,YC,VELOP(1:3),DEN,NDEN,PRES,TTEM,RTEM,VTEM,ToTEM,MACH,NEQ_PARAM(N),CELLSPOT(N)

800 CONTINUE
 CLOSE (Rank+11)
END SUBROUTINE

