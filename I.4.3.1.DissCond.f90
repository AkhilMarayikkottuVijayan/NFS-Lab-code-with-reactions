SUBROUTINE DISSCOND
USE VARIABLES
INTEGER         :: MAXLVL,L1,L2,Imax,MV
DOUBLE PRECISION:: Prob,SF,E_Red,MM1,MM2,FRAC,E_QK(2),E3

     L1      =       PP(9,CA(1))
     L2      =       PP(9,CA(2))

IF(SWITCH(L1,L2).EQ.1.AND.FLAG(9).EQ.1)THEN
 IF(QK_SWITCH.EQ.0)THEN

              IF(E_Redis.GT.E_DISSO(L1,L2))THEN
                IF(SP_NA(L1).EQ.1.AND.SP_NA(L2).GT.1)THEN
                        A1      =       CA(1)
                        A2      =       CA(2)
                        CA(1)   =       A2
                        CA(2)   =       A1
                        L1      =       PP(9,CA(1))
                        L2      =       PP(9,CA(2))
                END IF
                                 IF(SP_NA(L1).GT.1)THEN
                                     SF = COEF(L1,L2)*((E_Redis-E_DISSO(L1,L2))**&
                                      &NUME_EXP(L1,L2))/(E_Redis**DENO_EXP(L1,L2))
                                      WRITE(*,*) SF,SP_SY(L1),SP_SY(L2)
                                 IF(SF.GE.RAND(rank))THEN
                                      IF(SP_NA(L2).EQ.1.AND.DISS(CA(1)).NE.1) DISS(CA(1)) = 1
                                      IF(SP_NA(L2).EQ.2.AND.DISS(CA(2)).NE.1) DISS(CA(1)) =1
                                E_Redis = E_redis-E_DISSO(L1,L2)
                                DISS_MARK =1
                                 END IF
             END IF
   END IF
 END IF
END IF

END SUBROUTINE
