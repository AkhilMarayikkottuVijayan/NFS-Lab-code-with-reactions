SUBROUTINE PARADOMAIN
USE VARIABLES
USE MPI
INTEGER :: Q
!---------------------Sub Domain Definition Y Spilt--------------!
	H_SD = (OB(4)-OB(3))/(nprocs)
	W_SD = (OB(2)-OB(1))
	VOL_SD = H_SD*W_SD

	CB(1) = OB(1)
	CB(2) = OB(2)
	CB(3) = OB(3)+rank*H_SD
	CB(4) = CB(3)+H_SD
	IF(rank.EQ.(nprocs-1)) CB(4) = OB(4)

	REM		= MOD(NCY_D,nprocs)
	NCX_SD	= NCX_D
	NCY_SD   = NCY_D/nprocs
	IF(rank.EQ.(nprocs-1)) NCY_SD = NCY_SD + NINT(REM)
	
	NC_SD = NCY_SD*NCX_SD
	NSC_SD = NC_SD*NSCX*NSCY
	
	CW_SD = (CB(2)-CB(1))/NCX_SD 
	CH_SD = (CB(4)-CB(3))/NCY_SD

!----------------------------------------------------------------!

NC_D=NC_SD
NSC_D=NSC_SD
NCX_D=NCX_SD
NCY_D=NCY_SD
 CW = CW_SD
 CH = CH_SD
VOL_D=VOL_SD
END SUBROUTINE
