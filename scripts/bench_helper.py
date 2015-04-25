#!/usr/bin/env python3

f = open('./benches.conf', 'r')
for line in f.readlines():
    bname = line.split('=')[0].rstrip().lower()
    fname = ".smtbench_{}.ini".format(bname)
    bfile = open(fname, 'w')
    bmark_common = open("./.benchmark.ini", 'r')
    for bline in bmark_common.readlines():
        bfile.write(bline)
    bfile.write(line)
    bfile.close()
    bmark_common.close()

f.close()
