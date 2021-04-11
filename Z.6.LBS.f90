SUBROUTINE LBS(XMA,XMB,ERM)
100   ERM=RF(rank)				
	IF (XMA.LT.1.E-6.OR.XMB.LT.1.E-6) THEN	
		IF (XMA.LT.1.E-6.AND.XMB.LT.1.E-6) RETURN
		IF (XMA.LT.1.E-6) P=(1.-ERM)**XMB	
		IF (XMB.LT.1.E-6) P=(1.-ERM)**XMA	
	ELSE
		P=(((XMA+XMB)*ERM/XMA)**XMA)*(((XMA+XMB)*(1.-ERM)/XMB)**XMB)
	END IF
	IF (P.LT.RF(rank)) GO TO 100
RETURN
END SUBROUTINE
