CC = gcc
FC = pgf90
LINKER = pgf90
 

FLIB = libpspline_idl
IDL_LIB = /afs/localcell/usr/rsi/idl70/external

CFLAGS = -fPIC -D UNDERSCORE -I${IDL_LIB}
COBJ = pspline_idl.o

FFLAGS = -fpic
FOBJ = \
akherm1.o      gridspline.o     r8akherm2.o      r8herm2ev.o      r8splinck.o       splbrk.o  \
akherm2.o      gridtricub.o     r8akherm3.o      r8herm3ev.o      r8spline.o        spleen.o  \
akherm3.o      herm1ev.o        r8bcspeval.o     r8spvec.o         splinck.o \
bcspeval.o     herm2ev.o        r8bcspgrid.o     r8mkbicub.o      r8tcspeval.o      spline.o  \
bcspgrid.o     herm3ev.o        r8bcspline.o     r8mkbicubw.o     r8tcspgrid.o      spvec.o   \
bcspline.o     ibc_ck.o         r8bcspvec.o      r8mkherm1.o      r8tcspline.o      tcspeval.o    \
bcspvec.o          r8bpsplinb.o     r8mkherm2.o      r8tcspvec.o       tcspgrid.o    \
bpsplinb.o     mkbicub.o        r8bpspline.o     r8mkherm3.o      r8tpsplinb.o      tcspline.o    \
bpspline.o     mkbicubw.o       r8cspeval.o      r8mkintrp2d.o    r8tpspline.o      tcspvec.o \
cspeval.o      mkherm1.o        r8cspline.o      r8mkintrp3d.o    r8util_bcherm1.o  tpsplinb.o    \
cspline.o      mkherm2.o        r8cubsplb.o      r8mkspl2pb.o     r8util_bcherm2.o  tpspline.o    \
cubsplb.o      mkherm3.o        r8dnherm1.o      r8mkspl2p.o      r8util_bcherm3.o  util_bcherm1.o    \
dnherm1.o      mkintrp2d.o      r8dnherm2.o      r8mkspl2zb.o     r8vecbicub.o      util_bcherm2.o    \
dnherm2.o      mkintrp3d.o      r8dnherm3.o      r8mkspl2z.o      r8vecherm1.o      util_bcherm3.o    \
dnherm3.o      mkspl2pb.o       r8evbicub.o      r8mkspl3pb.o     r8vecherm2.o      vecbicub.o    \
evbicub.o      mkspl2p.o        r8evintrp2d.o    r8mkspl3zb.o     r8vecherm3.o      vecherm1.o    \
evintrp2d.o    mkspl2zb.o       r8evintrp3d.o    r8mkspline.o     r8vecintrp2d.o    vecherm2.o    \
evintrp3d.o    mkspl2z.o        r8evspline.o     r8mktricub.o     r8vecintrp3d.o    vecherm3.o    \
evspline.o     mkspl3pb.o       r8evtricub.o     r8mktricubw.o    r8vecpc1.o        vecin2d_argchk.o  \
evtricub.o     mkspl3zb.o       r8genxpkg.o      r8nspline.o      r8vecpc2.o        vecin3d_argchk.o  \
mkspline.o       r8gridbicub.o    r8pc1ev.o        r8vecpc3.o        vecintrp2d.o  \
mktricub.o       r8gridherm1.o    r8pc2ev.o        r8vecspline.o     vecintrp3d.o  \
genxpkg.o      mktricubw.o      r8gridherm2.o    r8pc3ev.o        r8vectricub.o     vecpc1.o  \
gridbicub.o    nspline.o        r8gridherm3.o    r8pspline.o      r8v_spline.o      vecpc2.o  \
gridherm1.o    pc1ev.o          r8gridintrp2d.o  r8seval2.o       r8xlookup.o       vecpc3.o  \
gridherm2.o    pc2ev.o          r8gridintrp3d.o  r8seval3.o       r8zonfind.o       vecspline.o  \
gridherm3.o    pc3ev.o          r8gridpc1.o      r8seval.o        seval2.o          vectricub.o   \
gridintrp2d.o  pspline_calls.o  r8gridpc2.o      r8speval.o       seval3.o          v_spline.o    \
gridintrp3d.o  pspline.o        r8gridpc3.o      r8spgrid.o       seval.o           xlookup.o \
gridpc1.o         r8gridspline.o   r8splaan.o       speval.o          zonfind.o    \
gridpc2.o      pspltsub.o       r8gridtricub.o   r8splbrk.o       spgrid.o \
gridpc3.o      r8akherm1.o      r8herm1ev.o      r8spleen.o       splaan.o 

MYFOBJ = calltest.o

#r8lookup_test.o  lookup_test.o f2test.o  f3test.o pspltest.o    

all: $(FLIB).so             #$(FLIB).a

$(FLIB).so:  $(FOBJ) $(MYFOBJ) $(COBJ)
	$(LINKER) -shared $(FFLAGS) -o $@ $(FOBJ) $(MYFOBJ) $(COBJ)
 

$(FLIB).a:  $(FOBJ) $(MYFOBJ) $(COBJ) 
	ar r $@ $(FOBJ) $(MYFOBJ) $(COBJ); ranlib $@

${COBJ}: %.o: %.c
	${CC} ${CFLAGS} -c -o $@ $<

${FOBJ}: %.o: ../pspline/%.f
	${FC} ${FFLAGS} -c -o $@ $<

${MYFOBJ}: %.o: %.f
	${FC} ${FFLAGS} -c -o $@ $<


clean:
	rm -f core.* *.o $(FLIB).a $(FLIB).so
