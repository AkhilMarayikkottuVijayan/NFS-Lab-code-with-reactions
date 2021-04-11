SUBROUTINE ADD_REMOVE
USE VARIABLES
INTEGER         	:: II,NMT,MSC,MC,L1,L2,X1,X2,Y2,unw
DOUBLE PRECISION        :: R1,R2,R3,i_comp,j_comp,k_comp,E_INTERNAL,Vint
DOUBLE PRECISION        :: Total_Mass

NMT=NM
DO 101 II=1,NMT
IF((DISS(II).EQ.1).OR.(DISS(II).EQ.3)) THEN
unw = PP(9,II)
        E_INTERNAL      =       PP(7,II)+PP(8,II)
		PP(7,II)=0
                PP(8,II)=0
		PP(10,II)=0
                X1 = PP(9,II)
                X2 = PDPS(1,X1)
                Y2 = PDPS(2,X1)
	        PP(9,II)= X2
                L1=PP(9,II)                

                NM=NM+1
               	MSC=IP(II)
        	MC = ISC(MSC)
                PP(1,NM)=CG(1,MC )+RF(0)*CG(3,MC )
                PP(2,NM)=CG(4,MC)+RF(0)*CG(6,MC )
                PP(9,NM)= Y2
                L2=PP(9,NM)
                PP(7,NM)=0
                PP(8,NM)=0
                PP(10,NM)=0

Total_Mass       =       SP(5,L1)+SP(5,L2)
Vint             =       SQRT(2*E_INTERNAL*Total_Mass/&
                         &(SP(5,L1)*SP(5,L2)))
R1               =       RF(rank)
R2               =       RF(rank)
R3               =       RF(rank)

                WRITE(555,*) E_INTERNAL,Vint,Total_Mass

        IF(R1.GE.0.5)THEN
                R1=1
        ELSE
                R1=-1
        END IF
        IF(R2.GE.0.5)THEN
                R2=1
        ELSE
                R2=-1
        END IF
        IF(R3.GE.0.5)THEN
                R3=1
        ELSE
                R3=-1
        END IF
                 
111                     i_comp  =       RF(rank)*R1
                        j_comp  =       RF(rank)*R2
                        IF((i_comp**2+j_comp**2).GE.0.99999999) GOTO 111
                        k_comp  =       SQRT(1-i_comp**2-j_comp**2)*R3
PP(4,II)   =    PP(4,II)+(SP(5,L2)*Vint*i_comp)/Total_Mass
PP(5,II)   =    PP(5,II)+(SP(5,L2)*Vint*j_comp)/Total_Mass
PP(6,II)   =    PP(6,II)+(SP(5,L2)*Vint*k_comp)/Total_Mass

                 PP(4,NM)   =     PP(4,II)-(SP(5,L1)*Vint*i_comp)/Total_Mass
                 PP(5,NM)   =     PP(5,II)-(SP(5,L1)*Vint*j_comp)/Total_Mass
                 PP(6,NM)   =     PP(6,II)-(SP(5,L1)*Vint*k_comp)/Total_Mass

                CALL CELLFINDER(NM,PP(1,NM),PP(2,NM))
	DISS(II)= 0
	DISS(NM)= 0

END IF
101 CONTINUE

END SUBROUTINE
                		
               		
