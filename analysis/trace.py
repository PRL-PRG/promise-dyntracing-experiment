import sys
import pprint


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
    parent = {}
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
            parent[promise_id] = stack[-1]
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
    print("children")
    pprint.pprint(children)
    print("parent")
    pprint.pprint(parent)
    del children[None]
    for parent_id, child_ids in children.items():
        for child_id in child_ids:
            child_cre = locations[child_id][0]
            parent_cre = locations[parent_id][0]
            parent_beg = locations[parent_id][1]
            if child_cre is None or parent_cre is None:
                continue
            if child_cre > parent_cre and child_cre < parent_beg:
                cannot_move.add(parent_id)

    print("Cannot move " + str(cannot_move))

    return locations, cannot_move


def compute_rewrite_order(trace):
    loc = -1
    exec_context = [None]
    parent_node_stack = [[("prb", None)]]
    forward_edge = {("prb", None): []}
    reverse_edge = {}
    locations = {}
    creation_context = {}

    def create_node(node):
        if node in forward_edge or node in reverse_edge:
            print("Problem", node, " already present in edge mappings")
        forward_edge[node] = []
        reverse_edge[node] = []

    def add_edge(parent_node, child_node, prepend=False):
        def add_edge_helper(parent_node, child_node, graph):
            if prepend:
                graph[parent_node].insert(0, child_node)
            else:
                graph[parent_node].append(child_node)
        add_edge_helper(parent_node, child_node, forward_edge)
        add_edge_helper(child_node, parent_node, reverse_edge)

    for instruction in trace:
        loc += 1
        opcode = instruction[0]
        if opcode not in ["prc", "prb", "prf", "prj"]:
            continue
        promise_id = instruction[1]
        cre_loc, beg_loc, fin_loc = locations.get(promise_id,
                                                  (None, None, None))
        if opcode == "prc":
            create_node(("prc", promise_id))
            locations[promise_id] = (loc, None, None)
            creation_context[promise_id] = exec_context[-1]
            parent_node = parent_node_stack[-1][-1]
            child_node = ("prc", promise_id)
            add_edge(parent_node, child_node)
            parent_node_stack[-1].append(child_node)
        elif opcode == "prb":
            create_node(("prb", promise_id))
            locations[promise_id] = (cre_loc, loc, None)
            current_node = ("prb", promise_id)
            if promise_id not in creation_context:
                add_edge(parent_node_stack[-1][-1], ("prb", promise_id))
            else:
                add_edge(("prc", promise_id), current_node, True)
                for context in reversed(exec_context):
                    if creation_context[promise_id] == context:
                        break
                    child_node = ("prb", context)
                    add_edge(current_node, child_node)
            exec_context.append(promise_id)
            parent_node_stack.append([current_node])
        elif opcode in ["prf", "prj"]:
            locations[promise_id] = (cre_loc, beg_loc, loc)
            exec_context.pop()
            parent_node_stack.pop()
            #parent_node_stack[-1].append(("prb", promise_id))
    print(max([len(value) for (key, value) in forward_edge.items()]))
    return (forward_edge, reverse_edge)


def topological_sort_depth_first(forward_edge, reverse_edge):
    topological_order = []
    permanently_marked = set()
    temporarily_marked = set()

    def visit(node):
        if node in permanently_marked:
            return
        if node in temporarily_marked:
            print("The graph still has edges, it means it has a cycle")
            return
        temporarily_marked.add(node)
        for child_node in reversed(forward_edge.get(node, [])):
            visit(child_node)
        temporarily_marked.remove(node)
        permanently_marked.add(node)
        topological_order.insert(0, node)

    visit(("prb", None))

    if permanently_marked != set(forward_edge.keys()):
        print("Some things have not been sorted!")

    return topological_order


def topological_sort_breadth_first(forward_edge, reverse_edge):
    topological_order = []
    parentless_nodes = [("prb", None)]

    while parentless_nodes:
        if len(parentless_nodes) > 1:
            print(parentless_nodes)
            print("Multiple topological orderings possible. Exiting...")
            #sys.exit(1)
        node = parentless_nodes.pop(0)
        print(node)
        topological_order.append(node)
        child_nodes = list(forward_edge.get(node, []))
        for child_node in child_nodes:
            reverse_edge[child_node].remove(node)
            forward_edge[node].remove(child_node)
            if not reverse_edge[child_node]:
                del reverse_edge[child_node]
                parentless_nodes.append(child_node)
            if not forward_edge[node]:
                del forward_edge[node]
    if len(forward_edge) or len(reverse_edge):
        print("The graph still has edges, it means it has a cycle")
    return (forward_edge, topological_order)


def can_move(promise_id, locations, parent, children):
    # A promise can be moved if all of its children that
    # can be moved will not be affected if this promise
    # is moved.
    # A promise is affected by movement if its forcing
    # point comes before creation point as a consequence.
    # This means that the promise is forced even before
    # its created. Such cases have to be avoided by
    # preventing the promise from being moved.
    pass


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
            if promise_id in cannot_move:
                continue
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


"""
prc 1


prc 2


prb 1
  prb 2
  prf 2
prf 1
"""


"""
prc 1
prc 2

prb 1
  prc 3
prf 1

prb 3
  prb 2
  prf 2
prf 3
"""
