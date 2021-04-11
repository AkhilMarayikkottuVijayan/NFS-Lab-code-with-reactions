SOURCES = A.0.Main.f90 A.1.Variables.f90\
B.0.PreSpecies.f90 B.1.Read.f90 B.2.PreReaction.f90 B.3.Exchange.f90\
C.0.Prepros.f90 C.1.PreBody.f90 C.2.PreGrid.f90 C.4.PreJet.f90 C.5.PreSamp.f90\
D.0.Report.f90 I.5.1.Dissociation.f90 I.6.2.Recombination.f90\
E.0.Checklist.f90 E.1.Diagnostics.f90\
F.0.Move.f90 F.1.Jetenter.f90 F.2.Refcond.f90 F.5.Bounds.f90 F.6.CLL.f90 F.7.Max.f90 F.8.REFIDOF.f90\
G.0.Adaptive.f90 G.1.Adapcond.f90 G.2.Adapaction.f90 G.3.Adapreport.f90\
H.0.Index.f90\
I.0.Collision.f90 I.1.Numcolls.f90 I.2.Colloccur.f90 I.3.Collselect.f90\
I.4.0.Collinel.f90 I.4.1.Vibcond.f90 I.4.2.Rotcond.f90 I.4.3.Revib.f90 I.4.4.Rerot.f90 I.4.5.Redtrans.f90\
I.4.3.1.DissCond.f90 I.4.3.2.ExchCond.f90\
I.5.0.Collel.f90 I.5.1.VHS.f90 I.5.2.VSS.f90\
J.0.Sample.f90 J.1.SampVDF.f90 J.2.Timeavg.f90\
K.0.Output.f90 K.1.Finalpos.f90 K.2.Velbins.f90 K.3.Macroprop.f90 K.4.Surfprop.f90 K.5.MacroSp.f90\
P.0.ParaInit.f90 P.1.ParaDomain.f90 P.2.ParaRepI.f90 P.3.ParaBounds.f90 P.4.ParaHalo.f90 P.5.ParaRepF.f90\
Z.0.Random.f90 Z.1.Rvelc.f90 Z.2.PCK.f90 Z.3.Cellfinder.f90 Z.4.Purge.f90 Z.5.Gamma.f90 Z.6.LBS.f90 Z.7.CellVol.f90 VelCollection.f90 KSTest.f90 DetectEqFcel.f90 EquiFlow.f90\

OBJECTS = A.0.Main.o A.1.Variables.o\
B.0.PreSpecies.o B.1.Read.o B.2.PreReaction.o B.3.Exchange.o\
C.0.Prepros.o C.1.PreBody.o C.2.PreGrid.o C.4.PreJet.o C.5.PreSamp.o\
D.0.Report.o I.5.1.Dissociation.o I.6.2.Recombination.o\
E.0.Checklist.o E.1.Diagnostics.o\
F.0.Move.o F.1.Jetenter.o F.2.Refcond.o F.5.Bounds.o F.6.CLL.o F.7.Max.o F.8.REFIDOF.o\
G.0.Adaptive.o G.1.Adapcond.o G.2.Adapaction.o G.3.Adapreport.o\
H.0.Index.o\
I.0.Collision.o I.1.Numcolls.o I.2.Colloccur.o I.3.Collselect.o\
I.4.0.Collinel.o I.4.1.Vibcond.o I.4.2.Rotcond.o I.4.3.Revib.o I.4.4.Rerot.o I.4.5.Redtrans.o\
I.4.3.1.DissCond.o I.4.3.2.ExchCond.o\
I.5.0.Collel.o I.5.1.VHS.o I.5.2.VSS.o\
J.0.Sample.o J.1.SampVDF.o J.2.Timeavg.o\
K.0.Output.o K.1.Finalpos.o K.2.Velbins.o K.3.Macroprop.o K.4.Surfprop.o K.5.MacroSp.o\
P.0.ParaInit.o P.1.ParaDomain.o P.2.ParaRepI.o P.3.ParaBounds.o P.4.ParaHalo.o P.5.ParaRepF.o\
Z.0.Random.o Z.1.Rvelc.o Z.2.PCK.o Z.3.Cellfinder.o Z.4.Purge.o Z.5.Gamma.o Z.6.LBS.o Z.7.CellVol.o VelCollection.o KSTest.o DetectEqFcel.o EquiFlow.o\

DSMC:
	mpif90 -c $(SOURCES)
	mpif90 -o DSMC $(OBJECTS) 

clean:
	rm -f *.o *.DAT *.phy *.dat
	rm -f DSMC
clean2:
	rm -f z-*.dat
