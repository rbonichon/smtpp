#!/usr/bin/env python3

import argparse
import re
import os
import logging
import configparser
import logging
import subprocess
import time
import itertools
import matplotlib, numpy
import matplotlib.pyplot as plt
import sys

config = configparser.ConfigParser(strict = True) # allow duplicate sections
config.read("./.benchmark.ini")

parser = argparse.ArgumentParser(
        "Comparing SMT tools")
parser.add_argument("-d", "--debug", dest = "debug",
                        action = "store_true",
                        help = 'activate debugging messages')
args = parser.parse_args()

if args.debug:
    logging.basicConfig(level=logging.DEBUG)
else:
    logging.basicConfig(level=logging.INFO)

benchmarks = []
smt2file = re.compile("\S+.smt2")
benchfiles = []

class Bench(object):
    def __init__(self, tool):
        self.toolname = tool["name"]
        self.toolcmd = tool["cmd"]
        self.total = 0
        self.results = dict()
        self.restypes = dict()

    def run(self, benchfile):
        cmd = "{0} {1}".format(self.toolcmd, benchfile)
#        logging.debug("{0}".format(cmd))
        self.total += 1
        tinit = os.times()
        output = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE).communicate()[0]
        tend = os.times()
        self.results[benchfile] = dict()
        self.results[benchfile]["output"] = output
        self.results[benchfile]["time"] = tend.children_user - tinit.children_user + tend.children_system - tinit.children_system
        try:
            self.restypes[output] += 1
        except KeyError:
            self.restypes[output] = 1


    def btime(self):
        ttime = 0
        for _, info in self.results.items():
            ttime += info["time"]
        return ttime

    def pp_summary(self, f):
        f.write('{0} ("{1}")\n'.format(self.toolname, self.toolcmd))
        f.write("{0} benchs done\n".format(self.total))
        avg = self.btime() / len(self.results)
        tmax = max([ info["time"] for (_, info) in self.results.items() ])
        tmin = min([ info["time"] for (_, info) in self.results.items() ])
        f.write("Avg time: {0}\n".format(avg))
        f.write("Max time: {0}\n".format(tmax))
        f.write("Min time: {0}\n".format(tmin))
        for k, n in self.restypes.items():
            f.write("{0} = {1}\n".format(k, n))

    def bench2list(self):
        l = [ (k, v) for (k, v) in self.results.items() ]
        l.sort()
        return l

    def timings(self):
        return [ v["time"] for (_, v) in self.bench2list() ]

    def benchnames(self):
        return [ os.path.basename(k) for (k, _) in self.bench2list() ]

    def plot(self, otherbench):
        l1 = self.bench2list()
        l2 = otherbench.bench2list()
        l3 = zip(l1, l2)
        data = [ x["time"]  / (y["time"] if y["time"] != 0 else 0.000000001) for ((_, x), (_, y)) in l3 ]
        fig = plt.figure()
        plt.plot(data)
        title = "{0} vs {1} (time)".format(self.toolname, otherbench.toolname)
        plt.title(title)
        fig.tight_layout()
        fname = "{0}_{1}.svg".format(self.toolname, otherbench.toolname)
        logging.debug("Writing {}".format(fname))
        plt.savefig(fname)

def mk_bench(tool, benchfiles):
    """ Launch the benchmark for a given tool with provided command """
    b = Bench(tool)
    n = len(benchfiles)
    for i, bf in enumerate(benchfiles):
        logging.debug("{1} / {2} : {0} {3}".format(b.toolname, i, n, bf))
        b.run(bf)
    b.pp_summary(sys.stdout)
    benchmarks.insert(0, b) # save it as a benchmark

def is_smt2_file(f):
    if re.match(smt2file, f): return True
    else: return False


for dtitle, dname in config['bench_directories'].items():
    print("Adding " + dname)
    for (dirpath, _, filenames) in os.walk(dname):
        for f in filenames:
            if is_smt2_file(f):
                benchfiles.insert(0, os.path.join(dirpath,f))

print("Added {} benchmarks".format(len(benchfiles)))

for _, toolname in config['tools'].items():
    try:
        tool = config[toolname]
        mk_bench(tool, benchfiles)
    except KeyError:
        logging.info("Tool description for {} not found".format(toolname))

plt.style.use('ggplot')

comb = itertools.combinations(benchmarks, 2)
#logging.debug("{0} combinations found".format(len(comb)))
for b1, b2 in comb:
    b1.plot(b2)


fig = plt.figure()
plt.title("Time comparison")
plt.margins(y=0.1)
plt.ylabel("Time(s)")
plt.xlabel('SMT-LIB benchmark')
#plt.xticks(range(len(benchfiles)), benchmarks[0].benchnames(), rotation=90)

for b in benchmarks:
    timings = b.timings()
    plt.plot(timings, label=b.toolname)

plt.legend()

fig.tight_layout()
fname = "tools.svg"
logging.debug("Writing {}".format(fname))
plt.savefig(fname)
