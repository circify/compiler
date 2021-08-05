#!/usr/bin/env python

import sys
import pandas as pd


def main():
    d = pd.read_csv("eval/zokrates/all_wide.csv")
    d.path = d.path.map(lambda p: '/'.join(p.split('/')[-2:]).split('.')[0])

    print("\\begin{tabular}[h]{lrrrr}")
    print("\\toprule")
    print("File & \multicolumn{2}{c}{CirC} & \multicolumn{2}{c}{ZoKrates}\\\\")
    print("& Constraints & Time (s) & Constraints & Time (s)\\\\")
    print("\\midrule")
    for i, r in d.iterrows():
        p = r.path.replace('_','\\_')
        print(f"{p} &{r.constraints_circify}&{r.wall_time_circify:0.2f}&{r.constraints_zokrates}&{r.wall_time_zokrates:0.2f}\\\\")
    print("\\bottomrule")
    print("\\end{tabular}")

main()

