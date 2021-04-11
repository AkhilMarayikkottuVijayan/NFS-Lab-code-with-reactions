SUBROUTINE EQUI
USE VARIABLES

DO I=1,NC_D
P_V(1,I)=0.0
P_V(2,I)=0.0
P_V(3,I)=0.0
PV1(1,I)=0.0
PV1(2,I)=0.0
PV1(3,I)=0.0
END DO
 
  DO 200 I=1,NM
          L=PP(9,I)
	  MSC=IP(I)		
 	  MC=ISC(MSC)			
 
            PV1(1,MC) =PV1(1,MC)+PP(4,I)
            PV1(2,MC) =PV1(2,MC)+PP(5,I)
            PV1(3,MC) =PV1(3,MC)+PP(6,I)
            
            P_V(1,MC) =P_V(1,MC)+PP(4,I)**2
            P_V(2,MC) =P_V(2,MC)+PP(5,I)**2
            P_V(3,MC) =P_V(3,MC)+PP(6,I)**2
        

  200  CONTINUE

   DO 300 N=1,NC_D
     
            P_V(1,N) =P_V(1,N)/IC(2,N,L)
            P_V(2,N) =P_V(2,N)/IC(2,N,L)
            P_V(3,N) =P_V(3,N)/IC(2,N,L)
      IF(IC(2,N,L).EQ.0)P_V(1,N)=0.0
      IF(IC(2,N,L).EQ.0)P_V(2,N)=0.0
      IF(IC(2,N,L).EQ.0)P_V(3,N)=0.0

        B_V(1,N) = PV1(1,N)/IC(2,N,L) 
        B_V(2,N) = PV1(2,N)/IC(2,N,L)
        B_V(3,N) = PV1(3,N)/IC(2,N,L)
      IF(IC(2,N,L).EQ.0)B_V(1,N)=0.0
      IF(IC(2,N,L).EQ.0)B_V(2,N)=0.0
      IF(IC(2,N,L).EQ.0)B_V(3,N)=0.0
   
   300  CONTINUE


    DO 400 I=1,NM
        MSC=IP(I)		
	     MC=ISC(MSC)	
        L=PP(9,I)
	
	          
        
  
         
  TEMPE=(SP(5,L)*((P_V(1,MC)-B_V(1,MC)**2.0)+(P_V(2,MC)-B_V(2,MC)**2.0)+(P_V(3,MC)-B_V(3,MC)**2.0)))/(3*BOLTZ)   
        
  
          
CALL TEMP(L,TEMPE,PP(4,I),PP(5,I),PP(6,I))
      
           DO J=1,3
               PP(3+J,I) = PP(3+J,I) + B_V(J,MC)
           ENDDO
	
    400 CONTINUE

END SUBROUTINE

SUBROUTINE TEMP(L,T,u,v,w)
USE VARIABLES
DOUBLE PRECISION::u,v,w,VMP1,T
VMP1= SQRT(2.*BOLTZ*T/SP(5,L))
        R1 = RF(rank)
        R2 = RF(rank)
        R3 = RF(rank)
        R4 = RF(rank)
	u = cos(2*PI*R1)*(SQRT(-Log(R2)))*VMP1
	v = sin(2*PI*R1)*(SQRT(-Log(R2)))*VMP1
	w = cos(2*PI*R3)*(SQRT(-Log(R4)))*VMP1

RETURN
	
END SUBROUTINE
