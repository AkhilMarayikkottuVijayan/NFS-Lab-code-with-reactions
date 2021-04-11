SUBROUTINE CLL(I)
USE VARIABLES
INTEGER	:: I
DOUBLE PRECISION	:: B0,VMPS,ALPN,ALPT
DOUBLE PRECISION 	:: R(3),TH(3),W(3)
DOUBLE PRECISION 	:: VNR,VTR(2),VTRI,VEANG,Vtan1,Ang
	L		=	PP(9,I)

	ALPN = Ac_CLLE(1,Bod) !!!KE Acc Co-efficient - normal component
	ALPT = Ac_CLLM(1,Bod)*(2-Ac_CLLM(1,Bod)) !!!!!KE Acc Co-efficient - tangential component
	VMPS  = SQRT(2.*BOLTZ*BODY_T(Bod)/SP(5,L))
		
	R(1) = SQRT(-ALPN*LOG(RF(rank))) 
	TH(1)= 6.283185308*RF(rank)
	!W(1) = ABS(Vnor/VMPS)*SQRT(1-ALPN)
	W(1) = (Vnor/VMPS)*SQRT(1-ALPN)
	VNR =VMPS*SQRT((R(1)**2)+(W(1)**2)+(2*R(1)*W(1)*COS(TH(1))))
        Vtan1= SQRT(Vtan**2+PP(6,I)**2)

	R(2)=SQRT(-ALPT*LOG(RF(rank)))
	TH(2)=6.283185308*RF(rank)
	!W(2) = ABS(Vtan/VMPS)*SQRT(1-ALPT)
	W(2) = (Vtan/VMPS)*SQRT(1-ALPT)

	VTR(1) = VMPS*(W(2)+(R(2)*COS(TH(2))))
	
        R(3) = SQRT(-ALPT*(LOG(RF(rank))))
        TH(3) = 6.283185308*RF(rank)
        VTR(2) = VMPS*R(3)*COS(TH(3))
PP(4,I) = (VTR(1)*CO)-(VNR*SI)
PP(5,I) = (VTR(1)*SI)+(VNR*CO)
PP(6,I) =PP(6,I)

IF(RF(rank).LT. ALFA_INT .AND. RF(rank) .LT. ALPN) THEN
	CALL REFINDF(BODY_T(Bod),L)
	PP(7,I)=ERS
	PP(8,I)=EVS
	
END IF

END SUBROUTINE
