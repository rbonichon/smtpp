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
import pickle
import sys


parser = argparse.ArgumentParser(
        "Comparing SMT tools")
parser.add_argument("-d", "--debug", dest = "debug",
                        action = "store_true",
                        help = 'activate debugging messages')
parser.add_argument("-f", "--file", dest = "configfile",
                    default=".benchmark.ini",
                    help = " use config file")
parser.add_argument("-pd", "--pickle-dir", dest = "pickledir",
                    default=".",
                    help = " load pickles located at directory")
parser.add_argument("-tout", "--timeout", dest = "timeout",
                    default="None",
                    help = " load pickles located at directory")


args = parser.parse_args()

if args.debug:
    logging.basicConfig(level=logging.DEBUG)
else:
    logging.basicConfig(level=logging.INFO)


# Reads configuration file
config = configparser.ConfigParser(strict = True) # allow duplicate sections
config.read(args.configfile)


# A list of Bench objects to be handled for later analysis
benchmarks = []
smt2file = re.compile("\S+.smt2")
# The list of files to be benchmarked
benchfiles = []

class Bench(object):
    def __init__(self, tool):
        self.toolname = tool["name"]
        self.toolcmd = tool["cmd"]
        self.total = 0
        self.results = dict()
        self.restypes = dict()
        self.timeouts = []

    def run(self, benchfile):
        cmd = "{0} {1}".format(self.toolcmd, benchfile)
#        logging.debug("{0}".format(cmd))
        self.total += 1
        tinit = os.times()
        sp = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
        try:
            if args.timeout is None:
                output, errs = sp.communicate()
            else:
                output, errs = sp.communicate(timeout=int(args.timeout))
            tend = os.times()
            time = tend.children_user - tinit.children_user + tend.children_system - tinit.children_system
        except subprocess.TimeoutExpired:
            sp.kill()
            output, errs = sp.communicate()
            self.timeouts.insert(0, benchfile)
            time = None

        logging.debug("{}, {}".format(sp.returncode, time))
        self.results[benchfile] = dict()
        self.results[benchfile]["output"] = output
        self.results[benchfile]["errs"] = errs
        self.results[benchfile]["ret"] = sp.returncode
        self.results[benchfile]["time"] = time
        try:
            self.restypes[sp.returncode] += 1
        except KeyError:
            self.restypes[sp.returncode] = 1

    def pfile(self):
        return "{}.pickle".format(self.toolname)

    def dump(self):
        pickle.dump(self, open(self.pfile(), "wb"))

    def copy(self, o):
        self.toolname = o.toolname
        self.toolcmd = o.toolcmd
        self.total = o.total
        self.results = o.total
        self.restypes = o.restypes

    def load_pickle(self):
        p = pickle.load(open(self.pickle(), "wb"))
        self.copy(p)

    def btime(self):
        ttime = 0
        for _, info in self.results.items():
            ttime += info["time"]
        return ttime

    def pp_summary(self, f):
        f.write('{0} ("{1}")\n'.format(self.toolname, self.toolcmd))
        res = [ (k, v) for (k, v) in self.results.items() if v["ret"] == 0 ]
        okbench = len(res)
        f.write("{1} / {0} benchs done\n".format(self.total, okbench))
        tok = 0
        for _, info in res:
            tok += info["time"]
        avg = tok / okbench
        tmax = max([ info["time"] for (_, info) in res ])
        tmin = min([ info["time"] for (_, info) in res ])
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
        def comp_time(v1, v2):
            if v1 == v2: return 1
            else:
                if v2 != 0:
                    # Handle special cases where timeout occurred
                    if v2 is None: return -1
                    elif v1 is None: return -2
                    return v1 / v2
                else: return v1 / 0.000000001
        l1 = self.bench2list()
        l2 = otherbench.bench2list()
        l3 = zip(l1, l2)
        data = [ comp_time(x["time"], y["time"]) for ((_, x), (_, y)) in l3 ]
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
    b.dump()
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
N = 10
params = plt.gcf()
plSize = params.get_size_inches()
params.set_size_inches( (plSize[0]*N, plSize[1]*N) )

fig.tight_layout()
fname = "tools.svg"
logging.debug("Writing {}".format(fname))
plt.savefig(fname)
