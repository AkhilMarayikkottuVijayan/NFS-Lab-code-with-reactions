SUBROUTINE  EXCHCOND(N)
USE VARIABLES
INTEGER                 ::      L1,L2,NPBE
DOUBLE PRECISION        ::      ECE,ENEFOR,PROBABILITY,Imax,FRAC,QQ
IF(DISS(CA(1)).EQ.0.AND.DISS(CA(2)).EQ.0)THEN
NPBE = NM
L1  = PP(9,CA(1))
L2  = PP(9,CA(2))
IF(EXRS(L1,L2).EQ.1)THEN
L3  = PRS(L1,L2,1)
L4  = PRS(L1,L2,2)
 IF(E_Redis.GT.ERAE(L1,L2))THEN
          PROBABILITY = PEF(L1,L2)*((E_Redis-ERAE(L1,L2))**(TE1(L1,L2)+TE2(L1,L2)))&
               &/(E_Redis**TE2(L1,L2))
          IF(PROBABILITY.GT.RAND(rank))THEN
                EXCH_MARK =1
                IF(SP_NA(L3).EQ.SP_NA(L1))THEN
                PP(9,CA(1)) = L3
                PP(9,CA(2)) = L4
!                WRITE(*,*) "EF1",SP_SY(L1),SP_SY(L2),SP_SY(L3),SP_SY(L4)
                ELSE
                PP(9,CA(1)) = L4
                PP(9,CA(2)) = L3
!                WRITE(*,*) "EF2",SP_SY(L1),SP_SY(L2),SP_SY(L3),SP_SY(L4)
                 END IF
                E_Redis = E_Redis-ERAE(L1,L2)+ERAEB(L1,L2)
           END IF
 END IF
END IF

IF(EXRS(L1,L2).EQ.2)THEN
L3  = PRS(L1,L2,1)
L4  = PRS(L1,L2,2)
Q_VIB(1) = 1
Q_VIB(2) = 1
QQ = 1

IF(CT(N).GT.200)THEN
 IF(SP_NA(L1).GT.1)THEN
   IF(CT(N).GT.SP(9,L1))THEN
     Q_VIB(1) = 1/(1-EXP((-SP(9,L1)/CT(N))))
   END IF
 END IF

 IF(SP_NA(L2).GT.1)THEN
   IF(CT(N).GT.SP(9,L2))THEN
     Q_VIB(1) = 1/(1-EXP((-SP(9,L2)/CT(N))))
   END IF
 END IF

                    IF(CT(N).GT.SP(9,L3))THEN
                        Q_VIB(2) = 1/(1-EXP((-SP(9,L3)/CT(N))))
                    END IF
                        QQ = Q_VIB(1)/Q_VIB(2)
                PROBABILITY = PEF(L1,L2)*1*((CT(N))**TE1(L1,L2))
        IF(PROBABILITY.GT.RAND(rank))THEN
        IF(E_Redis.GT.(ERAE(L1,L2)-ERAEB(L1,L2)))THEN
               EXCH_MARK =1
                IF(SP_NA(L3).EQ.SP_NA(L1))THEN
                PP(9,CA(1)) = L3
                PP(9,CA(2)) = L4
!               WRITE(*,*) "ER1",SP_SY(L1),SP_SY(L2),SP_SY(L3),SP_SY(L4)
                ELSE
                PP(9,CA(1)) = L4
                PP(9,CA(2)) = L3
!               WRITE(*,*) "ER2",SP_SY(L1),SP_SY(L2),SP_SY(L3),SP_SY(L4)
                 END IF
                E_Redis = E_Redis-ERAE(L1,L2)+ERAEB(L1,L2)
        END IF
        END IF  
           
END IF
END IF
END IF
END SUBROUTINE
