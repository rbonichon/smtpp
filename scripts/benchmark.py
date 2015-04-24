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
import shutil
import operator

# Main starts here
parser = argparse.ArgumentParser("Comparing SMT tools")
parser.add_argument("-d", "--debug", dest = "debug",
                        action = "store_true",
                        help = 'activate debugging messages')
parser.add_argument("-f", "--file", dest = "configfile",
                    default=".benchmark.ini",
                    help = " use config file")
parser.add_argument("-pd", "--pickle-dir", dest = "pickledir",
                    default=".",
                    help = " save/load directory for pickles")
parser.add_argument("-ld", "--load-dir", dest = "load",
                    action="store_true",
                    default=False,
                    help = " load pickles located at directory")
parser.add_argument("-t", "--timeout", dest = "timeout",
                    default=None,
                    help = " timeout for tools")
parser.add_argument("-g", "--graph", dest = "graph", default=False,
                    action="store_true",
                    help = " enables graph production")
parser.add_argument("-oat", "--one-at-time", dest = "oat", default=False,
                    action="store_true",
                    help = " make separate benches for directories")

args = parser.parse_args()

if args.debug:
    logging.basicConfig(level=logging.DEBUG)
else:
    logging.basicConfig(level=logging.INFO)

def dummy_tool ():
    d = dict()
    d["name"] = "dummy"
    d["bin"] = None
    d["cmd"] = None
    return d

class BenchError(Exception):
    def __init__(self, value):
        self.value = value
        Exception.__init__(self)

    def __str__(self):
        return repr(self.value)

class Bench(object):
    def __init__(self, tool):
        self.toolname = tool["name"]
        if (tool["bin"] and tool["bin"] is not None):
            binary = tool["bin"]
        else:
            binary = tool["cmd"]
        if binary is not None:
            if shutil.which(binary) is None:
                logging.debug("{} not found in PATH".format(binary))
                raise BenchError("{} not found".format(binary))
        self.toolcmd = tool["cmd"]
        self.total = 0
        self.results = dict()
        self.restypes = dict()
        self.timeouts = []
        self.benchname = ""

    def set_bench_name(self, name):
        self.benchname = name

    def run(self, benchfile):
        if self.toolcmd is not None:
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
        fname = self.toolname.replace(" ", "_")
        dirname = os.path.join(args.pickledir, self.benchname)
        if not os.path.exists(dirname):
            os.makedirs(dirname)
        return os.path.join(dirname, "{}.pickle".format(fname))

    def dump(self):
        pickle.dump(self, open(self.pfile(), "wb"))

    def copy(self, o):
        self.toolname = o.toolname
        self.toolcmd = o.toolcmd
        self.total = o.total
        self.results = o.results
        self.restypes = o.restypes

    def load_pickle(self, pfile):
        logging.debug("Loading pickle file {}".format(pfile))
        p = pickle.load(open(pfile, "rb"))
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
        avg = tok / okbench if okbench > 0 else -1
        tmax = -1
        tmin = -1
        if res != []:
            tmax = max([ info["time"] for (_, info) in res ])
            tmin = min([ info["time"] for (_, info) in res ])
        f.write("Avg time: {0}\n".format(avg))
        f.write("Max time: {0}\n".format(tmax))
        f.write("Min time: {0}\n".format(tmin))
        for k, n in self.restypes.items():
            f.write("{0} = {1}\n".format(k, n))

    def bench2list(self):
        #l = [ (k, v) for (k, v) in self.results.items() ]
        #l.sort()
        return sorted(self.results.items(), key=operator.itemgetter(0))

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


class Stat(object):
    def __init__(self, total):
        self.wins = 0
        self.total = total
        self.ko_wins = 0
        self.windiff_sum = 0
        self.windiff_max = 0
        self.max_name = None

    def win(self, diff, bname):
        if diff == -1:
            self.kowins += 1
        else:
            self.wins += 1
            self.windiff_sum += diff
            if diff > self.windiff_max:
                self.windiff_max = diff
                self.max_name = bname

    def avg_win(self):
        return self.windiff_sum / self.wins if self.wins > 0 else 0

    def all_wins(self):
        return self.wins + self.ko_wins

    def pp(self, f):
        twins = self.wins + self.ko_wins
        f.write("Wins : {} / {}\n".format(twins, self.total ))
        f.write("KO : {}\n".format(self.ko_wins))
        f.write("Avg win : {0:.4f} \n".format(
            self.windiff_sum / self.wins if self.wins > 0 else 0
        ))
        f.write("Max.win : {} ({})\n\n".format(
            self.windiff_max,
            self.max_name
        ))
        return

class StatMatch(object):
    def __init__(self, n1, n2, mlen):
        self.home = n1
        self.away = n2
        self.draws = 0
        self.total = mlen
        self.stats = dict()
        self.stats[n1] = Stat(mlen)
        self.stats[n2] = Stat(mlen)

    def register(self, home_bench, away_bench, name):
        if home_bench["ret"] >= 0:
            if away_bench["ret"] < 0:
                self.stats[self.home].win(-1, name)
            else:
                diff = abs(home_bench["time"] - away_bench["time"])
                if diff == 0:
                    self.draws += 1
                elif away_bench["time"] > home_bench["time"]:
                    self.stats[self.home].win(diff, name)
                else:
                    self.stats[self.away].win(diff, name)
        else:
            if away_bench["ret"] >= 0:
                self.stats[self.away].win(-1, name)
            else:
                self.draws += 1

    def winner(self):
        w1 = self.stats[self.home].all_wins ()
        w2 = self.stats[self.away].all_wins ()
        if w1 > w2:
            return self.home
        elif w2 > w1: return self.away
        else : return None

    def pp(self, f):
        f.write("# {} vs {} : {}\n".format(self.home, self.away, self.total))
        f.write("W/D/L: {} / {} / {}\n".format(self.stats[self.home].all_wins (),
                                             self.draws,
                                             self.stats[self.away].all_wins ()))
        f.write("Avg win: {}\n".format(self.stats[self.home].avg_win ()))
        f.write("Avg loss: {}\n".format(self.stats[self.away].avg_win ()))
        f.write("Best win: {0:.4f} ({1})\n".format(
            self.stats[self.home].windiff_max,
            self.stats[self.home].max_name))
        f.write("Worst loss: {0:.4f} ({1})\n".format(
            self.stats[self.away].windiff_max,
            self.stats[self.away].max_name))

        f.write("\n")

class Table():
    def __init__(self):
        self.ranks = dict()

    def add_score(self, name, v):
        if name in self.ranks:
            self.ranks[name] += v
        else:
            self.ranks[name] = v

    def register(self, statmatch):
        wname = statmatch.winner ()
        logging.debug("Winner between {} and {} is {}".format(
            statmatch.home,
            statmatch.away,
            wname))
        if wname is None:
            self.add_score(statmatch.home, 1)
            self.add_score(statmatch.away, 1)
        elif wname == statmatch.home:
            self.add_score(statmatch.home, 2)
            self.add_score(statmatch.away, 0)
        else:
            self.add_score(statmatch.home, 0)
            self.add_score(statmatch.away, 2)

    def pp(self, f):
        c = sorted(self.ranks.items(), key=operator.itemgetter(1))
        c.reverse()
        f.write("# Final standings\n")
        for i, (name, points) in enumerate(c, start = 1):
            f.write("{}. {:35s} : {}\n".format(i, name, points))
        f.write("\n")

class BenchCompare(object):
    """ Object to compare two benchmarks done on the same bench list """
    def __init__(self, b1, b2):
        self.bench1 = b1
        self.bench2 = b2
        logging.debug("Ready to compare {} and {}".format(self.bench1.toolname,
                                                   self.bench2.toolname))

    def compare_benches(self, standings=None):
        "Compute comparison stats with another bench"
        l1 = self.bench1.bench2list()
        l2 = self.bench2.bench2list()
        l = len(l1)
        sm = StatMatch(self.bench1.toolname, self.bench2.toolname, l)

        def comp(x, y):
            k1, b1 = x
            k2, b2 = y
            if k1 != k2:
                logging.info("Bench {} is not bench {}".format(k1, k2))
                return
            sm.register(b1, b2, k1)

        for x, y in zip(l1, l2):
            comp(x, y)
        sm.pp(sys.stdout)
        if standings is not None:
            standings.register(sm)
        return


smt2file = re.compile("\S+.smt2")
def is_smt2_file(f):
    if re.match(smt2file, f):
        return True
    else:
        return False

# Main starts here

if not os.path.exists(args.pickledir):
    os.makedirs(args.pickledir)

if args.load:
    pickles = []
    for (dirpath, _, fnames) in os.walk(args.pickledir):
        for f in fnames:
            if re.match(re.compile("\S+.pickle"),f):
                pickles.insert(0, os.path.join(dirpath, f))
    benches = []
    for p in pickles:
        b = Bench(dummy_tool ())
        b.load_pickle(p)
        benches.insert(0, b)
    standings = Table()
    for i, p in enumerate(benches, start=1):
        for p2 in benches[i:]:
                bc = BenchCompare(p, p2)
                bc.compare_benches(standings=standings)
    standings.pp(sys.stdout)

else:
    # Reads configuration file
    config = configparser.ConfigParser(strict = True) # allow duplicate sections
    config.read(args.configfile)

    # The list of files to be benchmarked
    benchfiles = [] if args.oat else [("all", [])]
    for dtitle, dname in config['bench_directories'].items():
        logging.debug("Adding " + dname)
        dirbenches = []
        for (dirpath, _, filenames) in os.walk(dname):
            for f in filenames:
                if is_smt2_file(f):
                    dirbenches.insert(0, os.path.join(dirpath,f))
        if args.oat:
            benchfiles.insert(0, (dtitle, dirbenches))
        else:
            benchfiles[0][1].extend(dirbenches)

    logging.debug("Added {} benchmarks".format(len(benchfiles)))

    def mk_toolbench(tool):
        try:
            b = Bench(tool)
        except BenchError as e:
            logging.info(e)
            b = None
        return b

    # Create bench objects for each tools if they exist
    tools = []
    for _, toolname in config['tools'].items():
        try:
            tool = config[toolname]
            b = mk_toolbench(tool)
            if b is not None:
                tools.insert(0, b)
        except KeyError:
            logging.info("Tool description for {} not found".format(toolname))

        # do the benchmark

    def mk_bench(toolbench, benchname, filenames):
        """ Launch the benchmark for a given tool with provided command """
        n = len(filenames)
        toolbench.set_bench_name(benchname)
        print(n)
        for i, bf in enumerate(filenames, start = 1):
            logging.debug("{1} / {2} : {0} {3}".format(toolbench.toolname, i, n, bf))
            toolbench.run(bf)
            toolbench.dump()
            toolbench.pp_summary(sys.stdout)
        return toolbench

    benches = [ mk_bench(t, n, b) for t in tools for n, b in benchfiles]


if args.graph:
    plt.style.use('ggplot')
    comb = itertools.combinations(benches, 2)
    #logging.debug("{0} combinations found".format(len(comb)))
    for b1, b2 in comb:
        b1.plot(b2)

    fig = plt.figure()
    plt.title("Time comparison")
    plt.margins(y=0.1)
    plt.ylabel("Time(s)")
    plt.xlabel('SMT-LIB benchmark')
    #plt.xticks(range(len(benchfiles)), benchmarks[0].benchnames(), rotation=90)

    for b in benches:
        logging.debug("Adding line for {}".format(b.toolname))
        timings = b.timings()
        plt.plot(timings, label=b.toolname)

    #plt.legend(bbox_to_anchor=(1.1, 1), loc=2)
    plt.legend(loc='upper left')
    N = 10
    params = plt.gcf()
    plSize = params.get_size_inches()
    #params.set_size_inches( (plSize[0]*N, plSize[1]*N) )

    fig.tight_layout()
    fname = "tools.pdf"
    logging.debug("Writing {}".format(fname))
    plt.savefig(fname)

# ends here
