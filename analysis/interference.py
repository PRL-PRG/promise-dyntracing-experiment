#!/usr/bin/env python3

import trace
import sys
import os
import abstract_interpreter
import pprint


def find_vignettes(input_dir, extension="trace"):
    files = [(os.path.splitext(filename)[0],
              os.path.getsize(os.path.join(input_dir, filename)))
             for filename in os.listdir(input_dir)
             if filename.endswith(extension)]
    return [filename for (filename, size) in
            sorted(files, key=lambda x: x[1])]


def compute_interference(input_dir, output_dir, vignette):
    output_path = os.path.join(output_dir, vignette)
    input_path = os.path.join(input_dir, vignette)
    lazy_trace_filename = input_path + ".trace"
    eager_trace_filename = output_path + "-eager.trace"
    lazy_state_filename = output_path + ".state"
    eager_state_filename = output_path + "-eager.state"
    state_difference_filename = output_path + ".difference"
    statistics_filename = os.path.join(output_dir, "interference.csv")

    def analyze(trace_filename, state_filename):
        print("Analyzing ", trace_filename)
        state = abstract_interpreter.eval_trace(trace_filename)
        with open(state_filename, 'wt') as outfile:
            pprint.pprint(state, stream=outfile)
        return state

    print("Converting ", lazy_trace_filename, " to ", eager_trace_filename)
    trace.to_eager_trace(lazy_trace_filename, eager_trace_filename)

    lazy_state = analyze(lazy_trace_filename, lazy_state_filename)
    eager_state = analyze(eager_trace_filename, eager_state_filename)

    print("Comparing ", lazy_state_filename, " and ", eager_state_filename)
    difference = abstract_interpreter.compare_states(lazy_state,
                                                     eager_state)
    with open(state_difference_filename, "wt") as outfile:
        pprint.pprint(difference, stream=outfile)

    with open(statistics_filename, "at") as csv_file:
        for (key, value) in difference["groups"].items():

            row = [vignette] + [element for tup in key
                                for element in tup] + [len(value)]
            csv_file.write(",".join(map(str, row)))
            csv_file.write("\n")


def main():
    input_dir = sys.argv[1]
    output_dir = sys.argv[2]
    vignettes = find_vignettes(input_dir)
    statistics_filename = os.path.join(output_dir, "interference.csv")
    with open(statistics_filename, "wt") as csv_file:
        csv_file.write("SCRIPT,VARIABLE NAME,CONTEXT TYPE,CONTEXT ID,LAZY CONTEXT TYPE,LAZY CONTEXT ID,EAGER CONTEXT TYPE,EAGER CONTEXT ID,COUNT\n")
    print("Found ", str(len(vignettes)), " trace files in ", input_dir)
    for vignette in vignettes:
        compute_interference(input_dir, output_dir, vignette)


if __name__ == "__main__":
    main()
