SUBROUTINE ADAPTIVE
USE VARIABLES
INTEGER :: Q
	LVL = 0

	NC_OLD=NC_D
	LVL = LVL+1
	CALL ADAPCOND
	
	CALL ADAPACTION
	CALL ADAPREPORT

END SUBROUTINE
