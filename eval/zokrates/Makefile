results.png: results.csv plot.R
	Rscript plot.R
clean:
	rm -rf tmp* results-wip.csv
all_wide.csv: all_results.csv widen.R
	cd ../../ && Rscript widen.R
