#!/bin/bash
F77=gfortran

FFLAGS="-c -O -w -fno-automatic"

LINK=$F77



LIBTOOLS=-lisared

CERNDIR=/home/adeel/Documents/cernlib/2006b/i686-slc5-gcc43-opt/
CERNBIN=$CERNDIR/bin/
CERNLIB=$CERNDIR/lib/

LFLAGS="-L$CERNLIB  -lpacklib -lmathlib -lkernlib"

#echo $FFLAGS

  $F77 $FFLAGS sugrun.f ;\
  $LINK -o isasugra.x sugrun.o ../aldata.o -L../ -lisajet $LIBTOOLS $LFLAGS
  

