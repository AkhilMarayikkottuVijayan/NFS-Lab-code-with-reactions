SUBROUTINE GRID
USE VARIABLES
INTEGER			  :: NX,NY,M,MX,MY
INTEGER ::P,Q,Count1,Count2
	DOUBLE PRECISION :: XI,YI,XF,YF
DOUBLE PRECISION :: REM,A,B,C,DET
!---------------------Domain Definition--------------------------!
	H_D = OB(4)-OB(3)
	W_D = OB(2)-OB(1)
	D_D = OB(6)-OB(5)
	VOL_D = H_D*W_D*D_D
	IF(D_D.EQ.0)VOL_D = H_D*W_D
!----------------------------------------------------------------!

CALL PARADOMAIN

!---------------------No. of Cells determination-----------------!
	NSIM = FND_D*VOL_D/FNUM
	NC_MAX = 4**FLAG(6)*NC_D
	NSC_MAX = NSCX*NSCY*NC_MAX
!----------------------------------------------------------------!

	ALLOCATE(CG(6,1:NC_MAX),CC(1:NC_MAX),CT(1:NC_MAX),CE(1:NC_MAX))
	ALLOCATE(CAR(1:NC_MAX),CAB(1:NC_D),CELLPOP(1:NC_D))
	ALLOCATE(CGN(6,1:NC_MAX))
	ALLOCATE(IC(2,1:NC_MAX,1:NSP_T),ISCG(2,1:NSC_MAX,1:NSP_T))
	ALLOCATE(NCOL(1:NC_MAX))
	ALLOCATE(CARC(2,NC_MAX,1:NSP_T,1:NSP_T))
	ALLOCATE(ISC(1:NSC_MAX),TRNS_T(1:NC_MAX))

!!!!!!!!!!!Allocation for Equlibrium Solver Variables!!!!!!!!!!!!!	
	ALLOCATE(P_V(3,1:NC_MAX))
	ALLOCATE(B_V(3,1:NC_MAX))
	ALLOCATE(PV1(3,1:NC_MAX))

!!!!!!!!!!!Allocation for KSTesT !!!!!!!!!!!!!
        ALLOCATE(NBIN(1:NC_MAX,1:1000),NEQ_PARAM(1:NC_MAX),CELLSPOT(1:NC_MAX),NMCELL(1:NC_MAX))
        ALLOCATE(NMC(1:NC_MAX),NMCT(1:NC_MAX),VELX(1:1000,1:NC_MAX))
        ALLOCATE(CBULK(1:NC_MAX))

	DO 95 N=1,NC_MAX
   		NMC(N)=0
		NMCT(N) = 0
                CBULK(N)=0
                NMCELL(N)=0
95	CONTINUE  

DO N=1,NC_D
        DO M=1,1000
                NBIN(N,M) = 0.0d0
        ENDDO
ENDDO

!---------------------Orthogonal grid generation-----------------!
	CG(1,1) = CB(1)
	CG(4,1) = CB(3)
	DO 100 NY=1,NCY_D
		DO 150 NX=1,NCX_D
			M = (NY-1)*NCX_D + NX
			IF (NX.EQ.1) CG(1,M) = CG(1,1)
			IF (NX.GT.1) CG(1,M) = CG(2,M-1)
			CG(2,M) = CG(1,M) + CW
			CG(3,M) = CG(2,M) - CG(1,M)
			IF (NY.EQ.1) CG(4,M) = CG(4,1)
			IF (NY.GT.1.AND.NX.EQ.1) CG(4,M) = CG(5,M-1)
			IF (NY.GT.1.AND.NX.GT.1) CG(4,M) = CG(4,M-1)
			CG(5,M) = CG(4,M) + CH
			CG(6,M) = CG(5,M) - CG(4,M)
			CT(M) = TMP_D
!                        CC(M) = CG(3,M)*CG(6,M)
		150	CONTINUE
100 CONTINUE
!----------------------------------------------------------------!
!---------------------Cell-Subcell Linkage-----------------------!	
		DO 400 N = 1,NC_D
		DO 450 MY = 1,NSCY
			DO 425 MX = 1,NSCX
				M=(N-1)*NSCX*NSCY+(MY-1)*NSCX+MX
				ISC(M)=N
425      CONTINUE
450   CONTINUE
400 CONTINUE

CALL CELLV
!----------------------------------------------------------------!
DO 300 L = 1, NSP_T
        DO 310 LL = 1, NSP_T
                IF((SP_EX(L).EQ.1).AND.(SP_EX(LL).EQ.1))THEN
                        DO 320 N = 1, NC_D
                                CARC(1,N,L,LL) = SQRT(BOLTZ*SPM(2,L,LL)/SPM(5,L,LL))*SPM(1,L,LL)
                                CARC(2,N,L,LL) = RF(rank)
320                     CONTINUE
                END IF
310     CONTINUE
300 CONTINUE

IF(rank.eq.0)WRITE(*,*)"PREPROSSING-GRID............. SUCCESSFUL"
END SUBROUTINE

