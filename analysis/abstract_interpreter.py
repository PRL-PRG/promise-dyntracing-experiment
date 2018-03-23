#!/usr/bin/env python3

import sys
from bisect import bisect_left, bisect_right
from pprint import pprint
from trace import parse_instruction

"""This is an abstract interpreter that records reads and writes
to variables inside of promises and functions."""

# if a promise writes to a variable, we need to consider the
# caller and callee for that promise as well. Do they both
# count as being responsible for side effects or only one does?
# For now, we can assume that both do.
# the final result will be


def promise_create(acc, location, promise_id, environment_id):
    acc["lookups"]["promise"][promise_id] = {}


def promise_begin(acc, location, promise_id):
    # push the promise id to the list of promises being forced
    acc["stack"].append(("promise", promise_id))


def promise_finish(acc, location, promise_id):
    # pop the promise id from the list of active promises
    entry = acc["stack"].pop()
    if not(entry[0] == "promise" and entry[1] == promise_id):
        print("Context mismatch")
        print("Current execution context is " + str(entry))
        print("Execution context exited from is " + str(("promise", promise_id)))
        sys.exit(1)

    # assign the parent promise id as the value of this promise
    # acc["env"]["p:" + str(promise_id)] = acc["active"][-1]


def promise_value_lookup(acc, location, promise_id):
    pass
    # promise_id = "p:" + str(promise_id)
    # # we look up the value of this promise in the environment
    # # If it is not found, we return a default value.
    # value = acc["env"].get(promise_id, "FREE PROMISE")
    # # get the id of the promise which is currently being forced
    # parent_promise_id = acc["stack"][-1]
    # # get the list of values looked up for this promise by the
    # # parent promise so far. If no lookup has been performed, then get
    # # an empty list
    # promise_lookups = acc["lookups"].get(parent_promise_id, {})
    # lookups = promise_lookups.get(promise_id, [])
    # # add the new looked up value to the list of values looked up
    # lookups.append(value)
    # # reassign the lookups list. This is only needed if
    # # this is the first lookup, but we do it in all cases
    # # because it is not incorrect to do so.
    # promise_lookups[promise_id] = lookups
    # acc["lookups"][parent_promise_id] = promise_lookups


def promise_expression_lookup(acc, location, promise_id):
    pass


def environment_create(acc, location, environment_id):
    pass


def environment_variable_lookup(acc, location, environment_id, variable_id,
                                value_type):
    # we look up the value of this variable in the environment.
    # If it is not found, we return a default value.
    if value_type in ["special", "built-in"]:
        return

    value = acc["env"].get(variable_id, "FREE VARIABLE")
    #print(acc["stack"])
    for execution_context in reversed(acc["stack"]):
        context_type = execution_context[0]
        context_id = execution_context[1]

        # get the list of values looked up for this variable by the
        # current execution context so far. If no lookup has been
        # performed, then get an empty list
        context_lookups = acc["lookups"][context_type].get(context_id, {})
        context_variable_lookups = context_lookups.get(variable_id, [])
        # add the new looked up value to the list of values looked up
        context_variable_lookups.append(value)
        # reassign the lookups list. This is only needed if
        # this is the first lookup, but we do it in all cases
        # because it is not incorrect to do so.
        context_lookups[variable_id] = context_variable_lookups
        acc["lookups"][context_type][context_id] = context_lookups
        return


def promise_context_jump(acc, location, promise_id):
    entry = acc["stack"].pop()
    if not(entry[1] == promise_id):
        print("Context mismatch")
        print("Current execution context is " + str(entry))
        print("Execution context exited from is " + str(("promise", promise_id)))
        sys.exit(1)


def environment_variable_remove(acc, location, environment_id, variable_id, value_type):
    # we remove the binding from the environment

    if variable_id in acc["env"]:
        del acc["env"][variable_id]
    else:
        print("variable", variable_id, "not found")


def environment_variable_assign(acc, location, environment_id, variable_id, value_type):
    # we assign the current scope as the value of the variable
    acc["env"][variable_id] = acc["stack"][-1]


def environment_variable_define(acc, location, environment_id, variable_id, value_type):
    # we assign the current scope as the value of the variable
    acc["env"][variable_id] = acc["stack"][-1]


def special_begin(acc, location, function_id, call_id, environment_id):
    acc["stack"].append(("special", call_id, function_id, environment_id))
    acc["lookups"]["special"][call_id] = {}


def special_finish(acc, location, function_id, call_id, environment_id):
    entry = acc["stack"].pop()
    if not(entry[0] == "special" and entry[1] == call_id):
        print(location)
        print("Context mismatch")
        print("Current execution context is " + str(entry))
        print("Execution context exited from is " + str(("special", call_id, function_id, environment_id)))
        sys.exit(1)


def builtin_begin(acc, location, function_id, call_id, environment_id):
    acc["stack"].append(("builtin", call_id, function_id, environment_id))
    acc["lookups"]["builtin"][call_id] = {}


def builtin_finish(acc, location, function_id, call_id, environment_id):
    entry = acc["stack"].pop()
    if not(entry[0] == "builtin" and entry[1] == call_id):
        print("Context mismatch")
        print("Current execution context is " + str(entry))
        print("Execution context exited from is " + str(("builtin", call_id, function_id, environment_id)))
        sys.exit(1)


def closure_begin(acc, location, function_id, call_id, environment_id):
    acc["stack"].append(("closure", call_id, function_id, environment_id))
    acc["lookups"]["closure"][call_id] = {}


def closure_finish(acc, location, function_id, call_id, environment_id):
    entry = acc["stack"].pop()
    if not(entry[0] == "closure" and entry[1] == call_id):
        print("Context mismatch")
        print("Current execution context is " + str(entry))
        print("Execution context exited from is " + str(("closure", call_id, function_id, environment_id)))
        sys.exit(1)


def function_context_jump(acc, location, call_id):
    entry = acc["stack"].pop()
    if not(entry[1] == call_id):
        print("Context mismatch")
        print("Current execution context is " + str(entry))
        print("Execution context exited from is " + str(("function", call_id)))
        sys.exit(1)


mappings = {"prc": promise_create,
            "prb": promise_begin,
            "prf": promise_finish,
            "prv": promise_value_lookup,
            "pre": promise_expression_lookup,
            "prj": promise_context_jump,
            "enc": environment_create,
            "enl": environment_variable_lookup,
            "enr": environment_variable_remove,
            "ena": environment_variable_assign,
            "end": environment_variable_define,
            "clb": closure_begin,
            "clf": closure_finish,
            "spb": special_begin,
            "spf": special_finish,
            "bub": builtin_begin,
            "buf": builtin_finish,
            "fnj": function_context_jump}


def analyze(acc, location, trace):
    mappings[trace[0]](acc, location, *trace[1:])


def eval_trace(trace_filename):
    location = 0
    acc = {"stack": [], #[("promise", -1)],
           "env": {},
           "lookups": {"promise": {},# { -1 : {}},
                       "special": {},
                       "builtin": {},
                       "closure": {}}}

    with open(trace_filename, "r") as trace_file:
        for line in trace_file:
            if not line.startswith("#"):
                analyze(acc, location, parse_instruction(line))
            location += 1

    return acc


def compare_states(state_a, state_b):
    state_a = state_a["lookups"]
    state_b = state_b["lookups"]

    difference = {"state": {},
                  "variables": []}
    for context_type in state_a.keys():
        print(context_type)
        if context_type not in state_b:
            print("Unable to find " + str(context_type) + " in state_b")
        #difference["state"][context_type] = {}
        context_a = state_a[context_type]
        context_b = state_b[context_type]
        for context_id in context_a.keys():
            if context_id not in context_b:
                print("Unable to find " + str(context_id) +
                      " in context " + context_type + " in state_b")
            #print(context_id)
            #difference["state"][context_type][context_id] = {}
            lookups_a = context_a[context_id]
            lookups_b = context_b[context_id]
            for variable_id in lookups_a:
                if variable_id not in lookups_b:
                    print("Unable to find " + str(variable_id) + " in context id " +
                          str(context_id) + " in context " + context_type + " in state_b")
                value_a = lookups_a[variable_id]
                value_b = lookups_b[variable_id]
                if value_a != value_b:
                    #difference["state"][context_type][context_id][variable_id] = (value_a, value_b)
                    difference["variables"].append((context_type, context_id, variable_id, (value_a, value_b)))

    # def compare(lookups_1, lookups_2):
    #     d = {}
    #     for key, lookups in lookups_1.items():
    #         if key not in lookups_2:
    #             pass
    #             #d[key] = (lookups, None)
    #         else:
    #             t = {}
    #             for variable_id in lookups.keys():
    #                 if variable_id not in lookups_2[key]:
    #                     t[variable_id] = (lookups[variable_id], None)
    #                 elif lookups[variable_id] != lookups_2[key][variable_id]:
    #                     t[variable_id] = (lookups[variable_id], lookups_2[key][variable_id])
    #             if len(t) != 0:
    #                 d[key] = t
    #     for key, lookups in lookups_2.items():
    #         if key not in lookups_1:
    #             pass
    #             #d[key] = (None, lookups)
    #     return d

    # difference = {}

    # for key in state_a["lookups"].keys():
    #     difference[key] = compare(state_a["lookups"][key],
    #                               state_b["lookups"][key])

    return difference


if __name__ == "__main__":
    acc = eval_trace(sys.argv[1])
    pprint(acc["lookups"])
