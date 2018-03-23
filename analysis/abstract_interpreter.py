import sys
from bisect import bisect_left, bisect_right
from pprint import pprint

def promise_created(acc, location, promise_id):
    pass

def promise_force_entry(acc, location, promise_id):
    acc['active'].append(promise_id)


def promise_force_exit(acc, location, promise_id):
    acc['active'].pop()
    acc['env']['p:' + str(promise_id)] = acc['active'][-1]


def promise_value_lookup(acc, location, promise_id):
    promise_id = 'p:' + str(promise_id)
    value = acc['env'].get(promise_id, 'FREE PROMISE')
    parent_promise_id = acc['active'][-1]
    promise_lookups = acc['lookups'].get(parent_promise_id, {})
    lookups = promise_lookups.get(promise_id, [])
    lookups.append(value)
    promise_lookups[promise_id] = lookups
    acc['lookups'][parent_promise_id] = promise_lookups


def variable_lookup(acc, location, variable_id):
    value = acc['env'].get(variable_id, 'FREE VARIABLE')
    promise_id = acc['active'][-1]
    promise_lookups = acc['lookups'].get(promise_id, {})
    lookups = promise_lookups.get(variable_id, [])
    lookups.append(value)
    promise_lookups[variable_id] = lookups
    acc['lookups'][promise_id] = promise_lookups


def variable_remove(acc, location, variable_id):
    if variable_id in acc['env']:
        del acc['env'][variable_id]


def variable_assign(acc, location, variable_id):
    acc['env'][variable_id] = acc['active'][-1]


def variable_define(acc, location, variable_id):
    acc['env'][variable_id] = acc['active'][-1]


def analyze(acc, location, trace):
    trace[0](acc, location, trace[1])


mappings = {'cre': promise_created,
            'ent': promise_force_entry,
            'ext': promise_force_exit,
            'val': promise_value_lookup,
            'rea': variable_lookup,
            'rem': variable_remove,
            'asn': variable_assign,
            'def': variable_define}


def parse(line):
    line = line.split(' ')
    return (
     mappings[line[0]], int(line[1]))


def eval_trace(trace_filename):
    location = 0
    acc = {'active': [-1],
           'env': {},
           'lookups': {}}
    with open(trace_filename, 'r') as (trace_file):
        for line in trace_file:
            if not line.startswith('#'):
                analyze(acc, location, parse(line))
            location += 1

    return acc


if __name__ == '__main__':
    acc = eval_trace(sys.argv[1])
    pprint(acc['lookups'])
