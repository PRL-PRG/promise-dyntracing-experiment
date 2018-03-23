from pprint import pprint


def str_to_instr(instr):
    name, arg = instr.strip().split(' ')
    return (
     name, int(arg))


def instr_to_str(instr):
    return instr[0] + ' ' + str(instr[1])


def read_trace(trace_filename):
    f = open(trace_filename, 'r')
    trace = [str_to_instr(instr) for instr in f.readlines() if not instr.startswith('#')]
    f.close()
    return trace


def write_trace(trace, trace_filename):
    f = open(trace_filename, 'w')
    for instr in trace:
        f.write(instr_to_str(instr) + '\n')

    f.close()


def extract_promise_locations(trace):
    locations = {}
    loc = 0
    for name, arg in trace:
        cre_loc, ent_loc, ext_loc = locations.get(arg, (None, None, None))
        if name == 'cre':
            locations[arg] = (
             loc, None, None)
        else:
            if name == 'ent':
                locations[arg] = (
                 cre_loc, loc, None)
            else:
                if name == 'ext':
                    locations[arg] = (
                     cre_loc, ent_loc, loc)
        loc += 1

    return locations


def to_eager_trace(lazy_trace):
    """
    Read the next instruction `inst` from `src[loc]`. Increment `loc`.
    -  If `inst` is `cre Pid`,  
       remove the entry for `Pid` in `promises`,
       it has the form `<loc0, loc1, loc2>`.  
       Push `<loc, loc1,  loc2>` on a stack.  
       increment `locDest`
       Copy `inst` to `dest[locDest]`
       Overwrite `src[loc] = null`
       set `loc = loc1`
    - If `inst` is `ext Pid`
      There is an entry `<loc0, loc1, loc2>` on the stack
      such that `loc2==loc`.  Remove that entry.
      Increment `locDest`
      Copy `inst` to `dest{locDest]`
      Overwrite `src[loc] = null`
      set `loc = loc0+1`
    - If `inst == null`
      Do nothing
    - Otherwise
      Increment `locDest`
      Copy `inst` to `dest{locDest]`
      Overwrite `src[loc] = null`
      overwrite `src[loc] = null`
    """
    loc_map = extract_promise_locations(lazy_trace)
    loc_stack = []
    eager_trace = []
    src_loc = 0
    dst_loc = 0
    while src_loc < len(lazy_trace):
        if lazy_trace[src_loc] is None:
            src_loc += 1
            continue
            name, arg = lazy_trace[src_loc]
            lazy_trace[src_loc] = None
            eager_trace.append((name, arg))
            src_loc += 1
            if name == 'cre':
                cre_loc, ent_loc, ext_loc = loc_map.get(arg, (None, None, None))
                loc_stack.append((cre_loc, ent_loc, ext_loc))
                if ent_loc is not None:
                    pass
            src_loc = ent_loc
        else:
            if name == 'ext' and loc_stack:
                cre_loc, ent_loc, ext_loc = loc_stack.pop()
                if ent_loc is not None:
                    src_loc = cre_loc + 1

    return eager_trace


def serialize_to_eager_trace(lazy_trace_filename, eager_trace_filename):
    write_trace(to_eager_trace(read_trace(lazy_trace_filename)), eager_trace_filename)
