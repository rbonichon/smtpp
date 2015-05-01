#!/bin/sh
set +x
TITLE=$1
DDIR=$2

# Only ready files are taken
# Useful if we want to generate a partial report out of running test session
MDFILES=`ls $DDIR/*.md | xargs grep END | cut -d ':' -f 1`
timestamp=`date +"%Y%m%d_%H:%M"`
base=`echo $TITLE | sed -e 's/ /_/g' | tr 'A-Z' 'a-z'`
prelude="$DDIR/prelude.md"

TFILE="title.md"
REPORTFILE="report.md"
TEXFILE="${base}_${timestamp}.tex"
PDFFILE=`echo $TEXFILE | sed -e 's/tex/pdf/'`

echo "% $TITLE ${timestamp}" > $TFILE

cat $TFILE $prelude $MDFILES > $REPORTFILE
pandoc -s -f markdown -t latex $REPORTFILE > $TEXFILE
pdflatex $TEXFILE
pdflatex $TEXFILE

rm -f $TEXFILE $TFILE *.aux *.log *.out

echo "You report is ready in $PDFFILE"
