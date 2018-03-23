#!/usr/bin/env python3

import trace
import sys
import os
import abstract_interpreter
import json


def find_trace_files(directory, extension="trace"):
    trace_files = []
    for dirpath, dirnames, filenames in os.walk(directory):
        trace_files.extend([(os.path.abspath(dirpath), filename)
                            for filename in filenames
                            if filename.endswith(extension)])
    return trace_files


def generate_eager_traces(trace_files):
    def generate_eager_trace_filename(filename):
        basename, extension = os.path.splitext(filename)
        eager_filename = basename + ".eager" + extension
        return eager_filename

    all_trace_files = []
    for (dirpath, filename) in trace_files:
        lazy_trace_filename = os.path.join(dirpath, filename)
        eager_trace_filename = generate_eager_trace_filename(
            lazy_trace_filename)
        print("Rewriting ", lazy_trace_filename,
              " to ", eager_trace_filename)
        trace.serialize_to_eager_trace(lazy_trace_filename,
                                       eager_trace_filename)
        all_trace_files.append((dirpath,
                                lazy_trace_filename,
                                eager_trace_filename))

    return all_trace_files


# def interpret(trace_files):
#     for (dirpath, lazy_trace_filename, eager_trace_filename) in trace_files:

def main():
    trace_dir = sys.argv[1]
    output_dir = sys.argv[2]
    trace_files = find_trace_files(trace_dir)
    print(trace_files)
    trace_files = generate_eager_traces(trace_files)
    # for (lazy_filename, eager_filename) in trace_files:
    #     print(difference)


def find_difference(vignette):
    print("Converting to eager trace")
    trace.to_eager_trace(vignette + ".trace",
                         vignette + ".eager.trace")
    print("Analyzing lazy trace")
    lazy_state = abstract_interpreter.eval_trace(
        vignette + ".trace")
    # with io.open('data.txt', 'w', encoding='utf-8') as f:
    #     f.write(json.dumps(data, ensure_ascii=False))
    # json.dumps()
    print("Analyzing eager trace")
    eager_state = abstract_interpreter.eval_trace(
        vignette + ".eager.trace")

    print("Computing Difference")
    difference = abstract_interpreter.compare_states(lazy_state,
                                                     eager_state)
    return difference


if __name__ == "__main__":
    main()
