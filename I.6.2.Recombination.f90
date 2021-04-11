SUBROUTINE RECOMBINE
USE VARIABLES
INTEGER                 :: N1,N2,N3,K,MMM
DOUBLE PRECISION        :: V_R(3),V_CM(3),E1,E2,E_REDIST

DO 1 I = 1, RCM
        CA(1) = RPAIR(I,1)
        CA(2) = RPAIR(I,2)
        N1    = PP(9,CA(1))
        N2    = PP(9,CA(2))
   
   DO 2 K = 4,6
        V_R(K-3) = PP(K,CA(1))-PP(K,CA(2))
   2 CONTINUE
        V_R_MAG = V_R(1)**2+ V_R(2)**2+ V_R(3)**2
        E1      = 0.5*SPM(5,N1,N2)*V_R_MAG

                DO 3  K=4,6
                    V_CM(K-3)   =(SP(5,N1)*PP(K,CA(1))+SP(5,N2)*PP(K,CA(2)))/(SP(5,N1)+SP(5,N2))
                    PP(K,CA(1)) =  V_CM(K-3)
                3 CONTINUE
                    KK           = PRPS(N1,N2)
                    PP(9,CA(1))  = PRPS(N1,N2)
                    MSTAT(CA(2)) = 1

                    MSC = IP(CA(1))
                    MC  = ISC(MSC)
                               IF( IC(2,MC,KK) .GT. 1 ) THEN
 30                                B= RF(rank)
                                   K=INT(B*(IC(2,MC,KK)-0.0001))+IC(1,MC,KK)+1
                                   M=IR(K)
                                      IF (M.EQ.CA(1).OR.M.EQ.CA(2)) GO TO 30
                                        N3    =  PP(9,M)
                                        CA(2) =  M
                                         DO 4  K=4,6
                                              VRC(K-3)=PP(K,CA(1))-PP(K,CA(2))
                                         4 CONTINUE
                                              V_R_MAG  = VRC(1)**2+ VRC(2)**2+VRC(3)**2
                                              E2      = 0.5*SPM(5,N1,N2)*V_R_MAG

                                        E_REDIST = E1+E2+E_DISSO(N1,KK)
                                
                                MAXLVL=E_REDIST/(BOLTZ*SP(9,L))
10                         PP(10,CA(1)) = RF(rank)*MAXLVL
                         E_Vibra(2) = INT(PP(10,CA(1)))*BOLTZ*SP(9,L)
                       Prob = (1-E_Vibra(2)/E_REDIST)**(RM(2)-1)
                      IF (Prob.LT.RF(rank)) GO TO 10
                     E_REDIST     = E_REDIST-E_Vibra(2)
                   PP(8,CA(1))     = E_Vibra(2)
                    
                     IF(SP(7,L).EQ.2)THEN
                        ERM = 1.-RF(rank)**(1./RM(2))
                     ELSE
                        RM(1) = 0.5*SP(7,L)
                        CALL LBS(RM(1)-1.,RM(2)-1.,ERM)
                     END IF
                     PP(7,CA(1)) = ERM*E_REDIST
                     E_Rotat(2)= PP(7,CA(1))
                     E_REDIST     = E_REDIST-E_Rotat(2)

                 A = SQRT(2.*E_REDIST/SPM(5,N1,N3))
        IF (ABS(SPM(4,N1,N3)-1.).LT.1.0E-3) THEN
                VR=A
        ELSE
                DO 300 K=1,3
                        VRC(K)=VRC(K)*A/VR
300     CONTINUE
                VR=A
        END IF
               CALL ECOLLS(N1,N3)
     END IF
1 CONTINUE

CALL PURGE


END SUBROUTINE



                                        
