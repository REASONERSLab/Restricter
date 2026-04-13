import argparse
import json
from collections import defaultdict
from copy import deepcopy
from random import seed, shuffle
from time import process_time

import restricter_sygus
import parse_schema
from get_pt import Solver


def build_parser():
    parser = argparse.ArgumentParser(description="Main program for running Restricter")
    parser.add_argument("--size", "-s", type=int, help="Problem Size", required=True)
    parser.add_argument("--repeat", "-r", type=int, help="Number of repeats for the experiment", default=1)
    parser.add_argument("--verbose", "-v", action="store_true", help="Print the grammar and solutions", default=False)
    parser.add_argument("--vv", action="store_true", help="Very verbose", default=False)
    parser.add_argument("--slice", type=int, help="Pick one rule to restrict", required=True)
    parser.add_argument("--case-study", type=str, help="Case study name", default="GClassroom")
    parser.add_argument("--entities_prefix", type=str, default="cedar-restrict-case-studies/dist/GClassroom/entities")
    parser.add_argument("--log_prefix", type=str, default="cedar-restrict-case-studies/dist/GClassroom/logs")
    parser.add_argument("--log_size", default=None, help="Size of log slice. If not provided, will use the full slice", type=int)
    parser.add_argument("--seed", type=int, default=196883, help="Random seed for shuffling")
    parser.add_argument("--num_req_negation", "-n", type=int, default=1, help="Number of requests to negate each loop")
    parser.add_argument('--num_conj', '-d', type=int, help='Number of conjunctions to synthesize', default=1)
    return parser


def resolve_case_study_files(case_study):
    if case_study == "GClassroom":
        return "gclassroom.schema.json", "gclassroom_policies.cedar"
    if case_study == "HotCRP":
        return (
            "cedar-restrict-case-studies/assets/HotCRP/schema.cedarschema.json",
            "cedar-restrict-case-studies/assets/HotCRP/policies.cedar",
        )
    raise ValueError(f"Unsupported case study: {case_study}")


def load_log(log_prefix, size):
    log = []
    with open(f"{log_prefix}.{size}.json") as f:
        raw_log = json.loads(f.read())
    for entry in raw_log:
        principal = entry["request"]["principal"]
        p_t, p_name = principal.split("::")
        p_name = p_name.replace("\"", "")
        action = entry["request"]["action"].split("::")[1].replace("\"", "")
        resource = entry["request"]["resource"]
        r_t, r_name = resource.split("::")
        r_name = r_name.replace("\"", "")
        context = entry["request"]["context"]
        decision = entry["result"] == "ALLOW"
        log.append(((p_t, p_name), action, (r_t, r_name), context, decision))
    return log


def main():
    parser = build_parser()
    args = parser.parse_args()

    policy_to_slice = args.slice
    size = args.size
    max_fail = 2

    schema_file, policy_file = resolve_case_study_files(args.case_study)
    entity_file = f"{args.entities_prefix}.{size}.json"

    with open(policy_file) as f:
        policy_text = f.read()

    if args.verbose:
        print("Parsing schema")
    with open(schema_file) as f:
        entity_schema, actions, action_scope, hierarchy = parse_schema.parse_schema(json.loads(f.read()))

    if args.verbose:
        print("OK")
        print("Parsing Entity store")
    with open(entity_file) as f:
        entity_store_json = json.loads(f.read())

    if args.verbose:
        print("OK")

    log = load_log(args.log_prefix, size)
    slices = defaultdict(list)
    pt_solver = Solver(entity_schema, actions, action_scope, hierarchy, entity_store_json, policy_text, verbose=False, vv=args.vv)

    # Slice the log
    for req in log:
        p, a, r, ctx, d = req
        if not d:
            continue
        for rule in range(pt_solver.num_rules):
            if pt_solver.evaluate_req(rule, p, a, r, ctx) == d:
                slices[f"policy{rule}"].append(req)

    print(f"size of log: {len(log)}")
    other_policies = [k for k in slices.keys() if k != f"policy{policy_to_slice}"]
    sliced = [
        req
        for req in slices[f"policy{policy_to_slice}"]
        if all(req not in slices[policy_name] for policy_name in other_policies)
    ]
    print("slice size: ", len(sliced))
    if not sliced:
        print("No slices found for this policy")
        raise SystemExit(1)

    if args.log_size is not None:
        seed(args.seed)
        shuffle(sliced)
        sliced = sliced[: args.log_size]
        print("Truncated slice size: ", len(sliced))

    pick_point_times = []
    fail_times = []
    success_times = []
    time_to_first_success = []
    total_times = []
    policies = []
    all_compl_pts = []

    # Run for args.repeat times to get some statistics
    for _ in range(args.repeat):
        blocked = deepcopy(sliced)
        success_time = []
        pick_point_time = []
        fail_time = []

        first_success = False
        num_fail = 0
        policy = policy_text
        last_policy = policy_text
        t_start = process_time()
        to_synth = policy_to_slice

        # repeat until sygus fail for max_fail times
        while policy:
            last_policy = policy
            if first_success:
                print(policy)
            to_synth = policy_to_slice if not first_success else 0

            # Pick requests in POP
            pt_solver = Solver(entity_schema, actions, action_scope, hierarchy, entity_store_json, policy, verbose=False, vv=args.vv)
            compl_list = []
            neg_generator = pt_solver.get_pt_generator(blocked, synth_rule=to_synth, check_log=False)

            for _ in range(args.num_req_negation):
                t0 = process_time()
                try:
                    point, unsat_core = next(neg_generator)
                except StopIteration:
                    print("Cannot find a request in POP")
                    if args.verbose:
                        print("Unsat core: ", unsat_core)
                    t2 = process_time()
                    print(f"Total time (no request to negate): {t2 - t_start}")
                    total_times.append(t2 - t_start)
                    break
                t1 = process_time()
                print("Request picking takes ", t1 - t0)
                pick_point_time.append(t1 - t0)
                print("Negating request", point)
                compl_list.append(point)
                all_compl_pts.append(point)
                blocked.append((*point, False))

            if not compl_list:
                print("No requests to negate")
                t2 = process_time()
                print(f"Total time (no requests to negate): {t2 - t_start}")
                total_times.append(t2 - t_start)
                break

            # main SyGuS call
            policy = restricter_sygus.solve(
                entity_schema,
                actions,
                action_scope,
                hierarchy,
                entity_store_json,
                sliced,
                policy,
                vars(args) | {"num_allows": None},
                verbose=args.verbose,
                vv=args.vv,
                synth_rule=to_synth,
                compl=compl_list,
            )
            t2 = process_time()

            # Policy is None if synthesis fails, otherwise it's the new policy
            if not policy:
                print(f"Time taken (fail): {t2 - t1}")
                fail_time.append(t2 - t1)
                num_fail += 1
                if num_fail >= max_fail:
                    print("Too many failures, stopping")
                    total_times.append(t2 - t_start)
                    break
                print("Retrying with last policy")
                policy = last_policy
            else:
                print(f"Time taken (success): {t2 - t1}")
                if not first_success:
                    time_to_first_success.append(num_fail)
                success_time.append(t2 - t1)
                first_success = True

        print("Final policy:")
        print(last_policy)
        policies.append(last_policy if first_success else None)

        pick_point_times.append(pick_point_time)
        fail_times.append(fail_time)
        success_times.append(success_time)
        print("Cannot improve further")

    # Analytics
    print("Request picking: ", [[t for t in run_times] for run_times in pick_point_times])
    print("Failure: ", [[t for t in run_times] for run_times in fail_times])
    print("Success: ", [[t for t in run_times] for run_times in success_times])
    print("Time to first success: ", time_to_first_success)
    print("Total times: ", [t for t in total_times])
    print("Policies: ", policies)


if __name__ == "__main__":
    main()
