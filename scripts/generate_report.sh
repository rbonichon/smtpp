#!/bin/sh
set +x
TITLE=$1
DDIR=$2

MDFILES=`ls $DDIR/*.md`
timestamp=`date +"%Y%m%d_%H:%M"`
base=`echo $TITLE | sed -e 's/ /_/g' | tr 'A-Z' 'a-z'`


TFILE="title.md"
REPORTFILE="report.md"
TEXFILE="${base}_${timestamp}.tex"
PDFFILE=`echo $TEXFILE | sed -e 's/tex/pdf/'`

echo "% $TITLE ${timestamp}" > $TFILE
echo "# Detailed results" >> $TFILE

cat $TFILE $MDFILES > $REPORTFILE
pandoc -s -f markdown -t latex $REPORTFILE > $TEXFILE
pdflatex $TEXFILE

rm -f $REPORTFILE $TEXFILE $TFILE *.aux *.log *.out

echo "You report is ready in $PDFFILE"
