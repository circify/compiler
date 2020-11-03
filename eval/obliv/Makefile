results.png: plot.R results.csv
	Rscript $<
results.csv: run.zsh
	zsh run.zsh
clean:
	rm -rf results-wip.csv tmp*
