SUBROUTINE REACTION
USE VARIABLES
INTEGER                 ::      LL1
ALLOCATE(TEMP_EXP(NSP_T,NSP_T),COEFF_REAC(NSP_T,NSP_T),E_DISSO(NSP_T,NSP_T),COEF(NSP_T,NSP_T))
ALLOCATE(NUME_EXP(NSP_T,NSP_T),DENO_EXP(NSP_T,NSP_T))
ALLOCATE(SYMM_FAC(NSP_T,NSP_T),SWITCH(NSP_T,NSP_T),INT_MODE(NSP_T,NSP_T))
ALLOCATE(PDPS(1:2,NSP_T))
        ALLOCATE(SWITCH_R(NSP_T,NSP_T),PRPS(NSP_T,NSP_T),RECOM_COEFF(NSP_T,NSP_T))
        ALLOCATE(RECOM_TEMP_EXP(NSP_T,NSP_T),CDT(NSP_T,NSP_T))
        ALLOCATE(QK_part(2))

                DO 100 M1=1,NSP_T
                  DO 101 M2=1,NSP_T
                        SWITCH(M1,M2)=0
                        SWITCH_R(M1,M2) = 0
                        RECOM_COEFF(M1,M2) = 0
                  101 CONTINUE
                100 CONTINUE 

OPEN(1,FILE="1.4.Reaction Database.txt",FORM="FORMATTED")
READ(1,*)
READ(1,*)
READ(1,*)  N_REAC
DO 2 NN=1,N_REAC
READ(1,*)
READ(1,*)
READ(1,*) SYM1,SYM2
        DO 21 M1=1,NSP_T
                IF(SYM1.EQ.SP_SY(M1))THEN
                  DO 23 M2=1,NSP_T
                     IF(SYM2.EQ.SP_SY(M2))THEN
                        READ(1,*) SWITCH(M1,M2)
                        READ(1,*) SYMM_FAC(M1,M2)
                        READ(1,*)COEFF_REAC(M1,M2)
                        READ(1,*)  TEMP_EXP(M1,M2)
                        READ(1,*) E_DISSO(M1,M2)
                        READ(1,*) INT_MODE(M1,M2)
                        READ(1,*) CDT(M1,M2)
                        READ(1,*) SYM3,SYM4
                                        DO 200 LL1 = 1,NSP_T
                                           IF(SYM3.EQ.SP_SY(LL1))THEN
                                                PDPS(1,M1) = LL1
                                           END IF
                                           IF(SYM4.EQ.SP_SY(LL1))THEN
                                                PDPS(2,M1) = LL1
                                           END IF
                                        200 CONTINUE
                                SWITCH(M2,M1)=SWITCH(M1,M2)
                                SYMM_FAC(M2,M1)=SYMM_FAC(M1,M2)
                                COEFF_REAC(M2,M1)=COEFF_REAC(M1,M2)
                                TEMP_EXP(M2,M1)= TEMP_EXP(M1,M2)
                                E_DISSO(M2,M1)=E_DISSO(M1,M2)
                                INT_MODE(M2,M1) =  INT_MODE(M1,M2)
COEF(M1,M2)=(SQRT(PI)* SYMM_FAC(M1,M2)*COEFF_REAC(M1,M2)*(SPM(2,M1,M2)**TEMP_EXP(M1,M2)))
COEF(M1,M2)=COEF(M1,M2)/(2*SPM(1,M1,M2)*((BOLTZ*SPM(2,M1,M2))**(&
            &TEMP_EXP(M1,M2)-1+SPM(3,M1,M2))))
COEF(M1,M2)=COEF(M1,M2)*(SQRT(SPM(5,M1,M2)/(2*BOLTZ*SPM(2,M1,M2))))
COEF(M1,M2)=COEF(M1,M2)*(GAM(SPM(7,M1,M2)+SPM(8,M1,M2)+2.5-SPM(3,M1,M2))/GAM(SPM(7,M1,M2)+SPM(8,M1,M2)+1.5+&
                &TEMP_EXP(M1,M2)))
        COEF(M2,M1)     =       COEF(M1,M2)
        NUME_EXP(M1,M2) =       TEMP_EXP(M2,M1)+INT_MODE(M1,M2)+0.5
        DENO_EXP(M1,M2) =       INT_MODE(M1,M2)-SPM(3,M1,M2)+1.5
        NUME_EXP(M2,M1) =       NUME_EXP(M1,M2)
        DENO_EXP(M2,M1) =       DENO_EXP(M1,M2)
                     END IF
                  23 CONTINUE
                END IF
        21 CONTINUE
2 CONTINUE

DO 1000 M1 =1, NSP_T
 DO 1001 M2 = 1,NSP_T
  IF(SWITCH(M1,M2).EQ.1)THEN
     WRITE(*,*) SP_SY(M1),SP_SY(M2)
     WRITE(*,*) COEF(M1,M2)
     WRITE(*,*) NUME_EXP(M1,M2)
     WRITE(*,*) DENO_EXP(M1,M2)
  END IF
 1001 CONTINUE
1000 CONTINUE


CLOSE(1)     

OPEN(2,FILE="1.5.Recombination Database.txt",FORM="FORMATTED")
READ(2,*)
READ(2,*)
READ(2,*) N_RECOM
  DO 3 I = 1,N_RECOM
       READ(2,*)
       READ(2,*)
       READ(2,*) SYM3,SYM4
         DO 31  M1 = 1,NSP_T
          IF(SYM3.EQ.SP_SY(M1)) THEN
             DO 32 M2 = 1,NSP_T
                IF(SYM4.EQ.SP_SY(M2))THEN
                  READ(2,*) SWITCH_R(M1,M2)
                  READ(2,*) RECOM_COEFF(M1,M2)
                  READ(2,*) RECOM_TEMP_EXP(M1,M2)
                  READ(2,*) SYM5
                        DO 301 LL1 = 1,NSP_T
                         IF(SYM5.EQ.SP_SY(LL1)) THEN
                           PRPS(M1,M2) = LL1
                         END IF
                        301 CONTINUE
                                SWITCH_R(M2,M1) = SWITCH_R(M1,M2)
                                RECOM_COEFF(M2,M1)= RECOM_COEFF(M1,M2)
                                RECOM_TEMP_EXP(M2,M1) = RECOM_TEMP_EXP(M1,M2)
                                PRPS(M2,M1) = PRPS(M1,M2)
                END IF
             32 CONTINUE
            END IF
         31 CONTINUE
  3 CONTINUE

CLOSE(2)
                           
!DO 1000 M1 = 1,4
!  DO 1001 M2 = 1,4
!    IF(SWITCH_R(M1,M2).EQ.1) THEN
!        WRITE(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!'
!        WRITE(*,*) M1,M2
!        WRITE(*,*) SP_SY(M1),SP_SY(M2)
!        WRITE(*,*) SWITCH_R(M1,M2)
!        WRITE(*,*) RECOM_COEFF(M1,M2),RECOM_TEMP_EXP(M1,M2)
!        WRITE(*,*) PRPS(M1,M2)
!        WRITE(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!'
!     END IF
!  1001 CONTINUE
!1000 CONTINUE
             

END SUBROUTINE         
