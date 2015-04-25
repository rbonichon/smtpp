# How to run a test

Once ran, the generated benchmarks will be saved as Python pickles
(i.e. serialized data) for further use as in any of the two sections below.

# How to generate the test configurations

1. In `benches.conf`, put the pairs title / directory that you
   want to test.
2. In `.benchmarks.ini`, prepare the list of tools you want to test, as well as
   their command and names (used for later pickle generation)
3. Run `make test_conf`. This should generate one `.smtbench_<title>.ini` file
for each directory you want to test.

If you wish, you can directly put all directories to be test as title /
directory pair under the `bench_directories` section of `.benchmarks.ini`. In
the current state of the benchmarks script, this is however less modular.

Running `make all_tests` will run `benchmarks.py` for all the `.*.ini` files of
the directories.


# How to compute a ranking for the test



# See the raw data
