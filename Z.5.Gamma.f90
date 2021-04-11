FUNCTION GAM(X)        ! calculates the Gamma function of X !
DOUBLE PRECISION :: X
     A=1.0
      Y=X
      IF (Y.LT.1.) THEN
        A=A/Y
      ELSE
5005      Y=Y-1
        IF (Y.GE.1.) THEN
          A=A*Y
          GO TO 5005
        END IF
      END IF
GAM=A*(1.-0.5748646*Y+0.9512363*Y**2-0.6998588*Y**3+0.4245549*Y**4-0.1010678*Y**5)
      RETURN
      END FUNCTION GAM
