import sys


def serialize_instruction(instruction):
    return " ".join(str(part) for part in instruction)


def parse_instruction(line):
    line = line.strip().split(" ")
    opcode = line[0]
    if opcode == "prc":
        return (opcode, int(line[1]), int(line[2]))
    elif opcode == "prb":
        return (opcode, int(line[1]), line[2])
    elif opcode in ["prf", "prv", "pre", "fnj", "prj"]:
        return (opcode, int(line[1]))
    elif opcode == "enc":
        return (opcode, int(line[1]))
    elif opcode in ["ena", "enl", "end", "enr"]:
        return (opcode, int(line[1]), int(line[2]), line[3], line[4])
    elif opcode in ["spb", "spf", "bub", "buf", "clb", "clf"]:
        return (opcode, line[1], int(line[2]), int(line[3]))
    else:
        print("Undefined opcode: ")
        print(line)
        print("Exiting ...")
        sys.exit(1)


def read_trace(trace_filename):
    f = open(trace_filename, 'r')
    trace = [parse_instruction(line) for line in f.readlines()
             if (not line.startswith('#')) ]
    f.close()
    return trace


def write_trace(trace, trace_filename):
    f = open(trace_filename, 'w')
    for instruction in trace:
        f.write(serialize_instruction(instruction) + '\n')
    f.close()


def extract_promise_locations(trace):
    locations = {}
    children = {}
    loc = -1
    stack = [None]
    for instruction in trace:
        loc += 1
        opcode = instruction[0]
        if opcode not in ["prc", "prb", "prf", "prj"]:
            continue

        promise_id = instruction[1]
        cre_loc, beg_loc, fin_loc = locations.get(promise_id,
                                                  (None, None, None))
        if opcode == "prc":
            locations[promise_id] = (loc, None, None)
        elif opcode == "prb":
            locations[promise_id] = (cre_loc, loc, None)
            if stack[-1] not in children:
                children[stack[-1]] = []
            children[stack[-1]].append(promise_id)
            stack.append(promise_id)
        elif opcode in ["prf", "prj"]:
            locations[promise_id] = (cre_loc, beg_loc, loc)
            stack.pop()

    normal = 0
    no_create = 0
    no_force = 0
    for (promise_id, (cre_loc, beg_loc, fin_loc)) in locations.items():
        if (cre_loc != None and beg_loc != None and fin_loc != None):
            normal += 1
        elif (cre_loc != None and beg_loc == None and fin_loc == None):
            no_force += 1
        elif (cre_loc == None and beg_loc != None and fin_loc != None):
            no_create += 1
        else:
            print("Promise " +
                  str(promise_id) +
                  " does not have correct configuration" +
                  str((cre_loc, beg_loc, fin_loc)))
            sys.exit(1)
    print("Number of normal promises: " + str(normal))
    print("Number of no create promises: " + str(no_create))
    print("Number of no force promises: " + str(no_force))

    cannot_move = set()

    del children[None]
    for parent_id, child_ids in children.items():
        for child_id in child_ids:
            child_cre = locations[child_id][0]
            parent_cre = locations[parent_id][0]
            parent_beg = locations[parent_id][1]
            if child_cre is None or parent_cre is None: continue
            if child_cre > parent_cre and child_cre < parent_beg:
                cannot_move.add(parent_id)

    print("Cannot move " + str(cannot_move))

    return locations, cannot_move


def to_eager_trace(lazy_trace_filename, eager_trace_filename):
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
    print("Loading Lazy Trace")
    lazy_trace = read_trace(lazy_trace_filename)
    print("Loaded Lazy Trace")
    prom_loc_map, cannot_move = extract_promise_locations(lazy_trace)
    loc_stack = []
    src_loc = 0
    jump_count = 0
    eager_trace_file = open(eager_trace_filename, 'w')
    eager_trace_len = 0

    while src_loc < len(lazy_trace):

        if lazy_trace[src_loc] is None:
            src_loc += 1
            continue

        instruction = lazy_trace[src_loc]
        opcode = instruction[0]
        lazy_trace[src_loc] = None
        src_loc += 1
        eager_trace_file.write(serialize_instruction(instruction) + '\n')
        eager_trace_len += 1

        if opcode == "prb":
            promise_id = instruction[1]
            lazy_trace[src_loc - 1] = ("prv", promise_id)

        elif opcode == "prc":
            promise_id = instruction[1]
            if promise_id in cannot_move: continue
            cre_loc, beg_loc, fin_loc = prom_loc_map[promise_id]
            if beg_loc is not None:
                loc_stack.append((promise_id, cre_loc, beg_loc, fin_loc))
                src_loc = beg_loc
                jump_count += 1

        elif (opcode == "prf" or opcode == "prj") and loc_stack:
                promise_id = instruction[1]
                if promise_id == loc_stack[-1][0]:
                    promise_id, cre_loc, beg_loc, fin_loc = loc_stack.pop()
                    if cre_loc is not None:
                        src_loc = cre_loc + 1
                else:
                    (cre_loc, beg_loc, fin_loc) = prom_loc_map[promise_id]
                    if cre_loc is not None:
                        print("Missing stack entry for promise " + str(promise_id))
                        print("Configuration is : " + str((cre_loc, beg_loc, fin_loc)))

    print("Length of lazy trace: " + str(len(lazy_trace)))
    print("Length of eager trace: " + str(eager_trace_len))
    print("Number of jumps: " + str(jump_count))
    print("Length of location map: " + str(len(prom_loc_map)))

    no_copy = 0
    for i in lazy_trace:
        if i is not None:
            no_copy += 1
            print(i)

    print("Number of no copy instructions is: " + str(no_copy))
    eager_trace_file.close()
