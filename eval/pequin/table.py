#!/usr/bin/env python

import sys
import pandas as pd


def main():
    d = pd.read_csv("eval/pequin/all_wide.csv")
    #d.path = d.path.map(lambda p: '/'.join(p.split('/')[0:]).split('.')[0])

    print("\\begin{tabular}[h]{lrrrr}")
    print("\\toprule")
    print("Benchmark & \multicolumn{2}{c}{CirC} & \multicolumn{2}{c}{Pequin}\\\\")
    print("& Constraints & Time (s) & Constraints & Time (s)\\\\")
    print("\\midrule")
    for i, r in d.iterrows():
        b = r.benchmark.replace('_','\\_')
        print(f"{b} &{r.constraints_circify}&{r.wall_time_circify:0.2f}&{r.constraints_pequin}&{r.wall_time_pequin:0.2f}\\\\")
    print("\\bottomrule")
    print("\\end{tabular}")

main()

