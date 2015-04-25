.phony: test_conf all_tests
test_conf:
	./bench_helper.py

RDIR ?= alltests
all_tests:
	for d in .*.ini; do \
		mkdir -p $(RDIR); \
		./benchmarks.py -f $$d -ld $(RDIR) -t 5 -oat; \
		echo $$d; \
	done
