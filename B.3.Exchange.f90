SUBROUTINE EXCHANGE_INIT
USE VARIABLES

INTEGER         ::      NER,LLL,MMM,NNN,OOO
DOUBLE PRECISION::      EXPC1,EXPC2,EXPC3
ALLOCATE(EXRS(NSP_T,NSP_T),PEF(NSP_T,NSP_T),TE1(NSP_T,NSP_T),TE2(NSP_T,NSP_T),ERAE(NSP_T,NSP_T))
ALLOCATE(PRS(NSP_T,NSP_T,2),EXPC(NSP_T,NSP_T),PNET(NSP_T,NSP_T),PDET(NSP_T,NSP_T))
ALLOCATE(ERAEB(NSP_T,NSP_T),CTEX(N),Q_VIB(2))

DO 2 LLL = 1,NSP_T
    DO 3 MMM = 1,NSP_T
        EXRS(LLL,MMM)   = 0
        PEF(LLL,MMM)    = 0
        TE1(LLL,MMM)    = 0
        TE2(LLL,MMM)    = 0
        PRS(LLL,MMM,1)  = 0
        PRS(LLL,MMM,2)  = 0
        ERAE(LLL,MMM)   = 0
     3 CONTINUE
2 CONTINUE


OPEN(1,FILE="1.6.Exchange Reaction.txt",FORM="FORMATTED")
READ(1,*)
READ(1,*)
READ(1,*)NER
DO 10 I = 1,NER
READ(1,*)
READ(1,*)
READ(1,*) SYM1,SYM2
        DO 100 LLL = 1,NSP_T
          IF(SYM1.EQ.SP_SY(LLL))THEN
             DO 200 MMM = 1,NSP_T
                IF(SYM2.EQ.SP_SY(MMM))THEN
                   READ(1,*) SYM3,SYM4
                      DO 300  NNN = 1,NSP_T
                        IF(SYM3.EQ.SP_SY(NNN))THEN
                           DO 400 OOO = 1,NSP_T
                              IF(SYM4.EQ.SP_SY(OOO))THEN
                                 READ(1,*) EXRS(LLL,MMM)
                                 EXRS(MMM,LLL) = EXRS(LLL,MMM)
                                 READ(1,*) PEF(LLL,MMM)
                                 PEF(MMM,LLL) = PEF(LLL,MMM)
                                 READ(1,*) TE1(LLL,MMM)
                                 TE1(MMM,LLL) = TE1(LLL,MMM)
                                 READ(1,*) TE2(LLL,MMM)
                                 TE2(MMM,LLL) = TE2(LLL,MMM)
                                 PRS(LLL,MMM,1) = NNN
                                 PRS(MMM,LLL,1) = NNN
                                 PRS(MMM,LLL,2) = OOO
                                 PRS(LLL,MMM,2) = OOO
                                 READ(1,*)  ERAE(LLL,MMM)
                                 ERAE(MMM,LLL)  = ERAE(LLL,MMM)
                                 READ(1,*)  ERAEB(LLL,MMM)
                                 ERAEB(MMM,LLL) = ERAEB(LLL,MMM)
                              END IF
                           400 CONTINUE
                         END IF
                       300 CONTINUE
                  END IF
               200 CONTINUE
          END IF
        100 CONTINUE
10 CONTINUE

READ(1,*)
READ(1,*) NER
DO 20 I = 1,NER
READ(1,*)
READ(1,*)
READ(1,*) SYM1,SYM2
        DO 21 LLL = 1,NSP_T
          IF(SYM1.EQ.SP_SY(LLL))THEN
             DO 22 MMM = 1,NSP_T
                IF(SYM2.EQ.SP_SY(MMM))THEN
                   READ(1,*) SYM3,SYM4
                      DO 23  NNN = 1,NSP_T
                        IF(SYM3.EQ.SP_SY(NNN))THEN
                           DO 24 OOO = 1,NSP_T
                              IF(SYM4.EQ.SP_SY(OOO))THEN
                                 READ(1,*) EXRS(LLL,MMM)
                                 EXRS(MMM,LLL) = EXRS(LLL,MMM)
                                 READ(1,*) PEF(LLL,MMM)
                                 PEF(MMM,LLL) = PEF(LLL,MMM)
                                 READ(1,*) TE1(LLL,MMM)
                                 TE1(MMM,LLL) = TE1(LLL,MMM)
                                 PRS(LLL,MMM,1) = NNN
                                 PRS(MMM,LLL,1) = NNN
                                 PRS(MMM,LLL,2) = OOO
                                 PRS(LLL,MMM,2) = OOO
                                 READ(1,*)  ERAE(LLL,MMM)
                                 ERAE(MMM,LLL)  = ERAE(LLL,MMM)
                                 READ(1,*)  ERAEB(LLL,MMM)
                                 ERAEB(MMM,LLL) = ERAEB(LLL,MMM)
                              END IF
                           24 CONTINUE
                         END IF
                       23 CONTINUE
                  END IF
               22 CONTINUE
          END IF
        21 CONTINUE
20 CONTINUE
 
 
CLOSE(1)

!DO 1000 I = 1,NSP_T
! DO 1001 J = 1,NSP_T
!   IF(EXRS(I,J).EQ.1)THEN
!      WRITE(*,*) I,SP_SY(I)
!      WRITE(*,*) J,SP_SY(J)
!      WRITE(*,*) PRS(I,J,1),SP_SY(PRS(I,J,1))
!      WRITE(*,*) PRS(I,J,2),SP_SY(PRS(I,J,2))
!      WRITE(*,*) PEF(I,J), TE1(I,J),TE2(I,J),ERAE(I,J)
!      WRITE(*,*) "______________________________________"
!    END IF
!  1001 CONTINUE
!1000 CONTINUE

END SUBROUTINE
