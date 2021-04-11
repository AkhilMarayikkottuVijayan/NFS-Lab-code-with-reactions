SUBROUTINE NCOLLS(N,L,LL,NSEL)
USE VARIABLES	
INTEGER :: N,L,LL
INTEGER :: SN,Navg,Nins
DOUBLE PRECISION :: ASEL,Pmax
INTEGER :: NSEL
	SN = CS(1,N,L) 
	IF(SN.GT.1) THEN
		Navg = FLOAT(SN/NSMP)
	ELSE
		Navg = IC(2,N,LL)
	END IF
	Nins = IC(2,N,LL)
	Pmax = FNUM*DTM*(CARC(1,N,L,LL)/CC(N))
	IF(CC(N).EQ.0) Pmax=0
        IF(FLAG(8) .GE. 2 .AND. CELLSPOT(N).EQ. 0 .AND. NPR .GT. NTS(1)) Nins=(Nins/2) ! For Equilibrium Cell 
	ASEL = (0.5*Navg*Nins*Pmax)+CARC(2,N,L,LL)
	NSEL = ASEL
	CARC(2,N,L,LL) = ASEL-NSEL 
RETURN
END SUBROUTINE