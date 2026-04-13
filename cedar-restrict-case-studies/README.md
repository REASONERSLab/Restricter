# Restrictor Case Study Generator

## Overview and Usage

### Terminology
**Families of datasets**
Case studies are generated as "families", which are sets of related entity
stores and logs, to facilitate the evaluation of how well Restrictor scales. Each
member of a family is associated with a size. At a first approximation, for two
members `m1` and `m2` of the same family such that the size of `m1` < size of
`m2`, every entity appearing in the store for `m1` appears in the store for
`m2`, and every log entry appearing in the log for `m1` appears in the log of
`m2`.

**Representation of exercised privileges in logs**

Each case studies for Restrictor are structured around two policies:
- an implicit "ideal" policy, which we wish Restrictor to discover
- an explicit "actual" policy, deliberately written to permit more requests than
  the ideal policy. 

We maintain a bijection between the rules of the ideal and actual policies, such
that:
- permit (resp. deny) rules of the ideal policy correspond to permit (resp.
  deny) rules of the actual policy;
- for each permit rule of the ideal policy, every request accepted by the rule
  is accepted by its corresponding rule in the actual policy;
- the bijection relating deny rules between the two policies is identity.

As an example, in the Google classroom case study, the ideal policy does not
permit TAs to ever edit assignments, but the actual policy does; the ideal
policy permits teachers to edit assignments, and so does the actual policy.

For each rule of a policy (either ideal or actual), call the set of all requests
(triples `(principal, action, resource)`) for which the rule applies, **and**
for which the policy accepts the request, the *privilege* afforded by the rule.
For each pair of rules `(r1,r2)` where `r1` is a rule from the ideal policy and
`r2` is the corresponding rule from the actual policy:
- the **ideal privilege** is the privilege of `r1`;
- the **actual privilege** is the privilege of `r2`;
- the **over privilege** is the set difference of the privilege of `r2` and
  `r1`.

We call such pairs of rules *corresponding rules,* with `r1` the *ideal rule*
and `r2` the *actual rule*. Henceforth, we leave implicit quantification over
corresponding rules implicit when this is unambiguous.

- The **privilege representation** of a rule `r` in a log is the set (more
  accurately, *multiset*) of requests in that log that are members of the
  privilege of `r`.
- The **ideal privilege representation** and **over privilege representation**
  are resp. the privilege representations for the ideal privilege of `r1` and
  the over privilege of `r2` in a log.

**Shared command line arguments**

The case-study generator is multi-mode, with each mode corresponding to a case
study (`gc` for Google classroom, `pm` for project management, and `hc` for
HotCRP). Each mode takes case-study specific arguments, and produces a family of
entity stores and logs. Some command line arguments are shared across all modes,
and are important for understanding the process for entity and log family
generation.

- `--size-start=INT` `--size-incr=INT` `--size-end`

  Specifies the start, end, and increment of the dataset size parameters. Each
  size calculated from these parameters specifies the size associated with some
  family member, with the number of family members determined roughly by
  `1 + (end - start)/incr`

- `--seed`

  Sets the seed for PRNG for reproducibility (default: `2025`).

- `--priv-rep-ratio=NUM`

   Upper bound on the ratio of the actual privilege representation to the actual
   privilege for each corresponding pair of rules. It is an upper bound due to
   - rounding down
   - possible duplicates (less likely if this is small --- keep the birthday
     paradox in mind!)

   Note that for some case studies, this general default may be overridden for
   some corresponding pairs.

- `--over-priv-percent=INT`

   An upper bound on the percentage of the actual privilege representation that
   is over privilege representation. Values range from 0 to 49.


### Entity Generation

Each case study has some invariants imposed on entity generation to make the
synthetic datasets more realistic. For example, for the Google classroom case
study there can never be courses for which there is no teacher (we make the
simplifying assumption that each course has exactly one teacher). The general
organizing principle used across case studies for generating entity stores is to
make a distinction between "primary", "secondary", and (optionally) "tertiary"
entity types.

- A *primary entity type* is one for which the number of entities generated of
  that type is controlled directly by the family size and (optionally) a ratio
  argument.

  - For the Google classroom example, these entity types are: students,
    teachers, and TAs (technically, Staff, where teachers and TAs are
    distinguished by their role attribute, but we informally treat teachers and
    TAs as separate entity types here).

  - For the project management example, these are projects and all users save
    those designated project managers

- A *secondary entity type* is one for which the realism-maintaining invariants
  require that the number of such entities be a (random) function of the number
  of entities of one or more primary or secondary entity types. The command line
  interface allows the the number of such entities to be influenced by an upper
  bound on coefficients

  - For the Google classroom example, these entity types are:
    - courses: each teacher teaches a random number of courses from `1` to
      `max-teacher-courseload`
    - assignments: each course has a random number of assignments from `1` to
      `max-assignments-per-course`
  - For the project management example, these are the users designated project
    managers
    - each project has exactly one manager (enforced by the entity model)
    - each user designated as a project manager is in exactly one project

- Finally, a *tertiary entity type* is one for which invariants and the
  relations between primary and secondary entity types completely determines the
  number of entities.

  For the Google classroom example, only grades fall into this category (each
  course corresponds to a disjoint set of grades, where each grade corresponds
  to a pair consisting of an assignment for the course and a student enrolled in
  the course. Note that disjointness of course grades is a consequence of the
  imposed invariant that course assignments are disjoint for different courses).

Of course, the entity store also stores the *relations* between entities, not
just entities themselves. These can be classified similarly to entities.

- A *primary entity relation* is an invariant used in entity generation, and so
  is not malleable (save for the number of entities being related).

  - In the Google classroom example, this is:
    - every course has exactly one teacher
    - every assignment has exactly one course (enforced by the entity model)
    - a student-assignment pair corresponds to at most one grade
    - a grade corresponds to exactly one student-assignment pair (enforced by the
      entity model)
  - In the project management example, this is:
    - there is a bijection between projects and users designated as project
      managers

- A *secondary entity relation* is a one-to-many relation (one primary entity to
  many entities), with the upper bound on the cardinality of "many" set by a
  command line argument.

  - In the Google classroom example, this is:
    - each teacher, TA, and student is in at least one course (upper bounds are
      set by `--max-teacher-courseload`, `--max-ta-courseload`,
      `--max-student-courseload`)
  - In the project management example, this is:
    - every non-project-manager user is in at least one project (upper bound is
      set by `--max-user-projectload`)

- Finally, a *tertiary entity relation* is a relation directly influenced by one
  or more primary or secondary entity relations, and is thus only indirectly
  influenced by the user (direct influence by the user could lead to
  over-constraining or unsatisfiability). These are typically duals of secondary
  entity relations, and are otherwise difficult to characterize.

  In the Google classroom example:
  - there is no restriction on the number of TAs per course or an upper bound on
    the number of students per course.
    - Maximums for either could lead failure to associate some students or TAs
      with courses, which would require backtracking and culling (making these
      primary entity types less directly influenced by the user);
    - A minimum number of TAs for course could conflict with the upper bound on
      TA courseloads.

### Log Generation

Setting aside considerations for families, log generation is comparatively
straightforward. In general, it is controlled by two parameters,
`--priv-rep-ratio` and `--over-priv-percent`. For each corresponding pair of
rules `(r1,r2)`, the space of ideal privilege is randomly sampled (floor of)
`|P(r1)| * R * (100-O)/100` times, where
- `P(r)` is the privilege of rule `r`
- `R` is the ratio set by `--priv-rep-ratio`
- `O` is the percentage set by `--over-priv-percent`

The set of requests produces by this sampling is the *ideal privilege representation.*

Then, the space of over privilege is sampled (floor of) `|P(r1)| * R * O/100`
times, producing the *over privilege representation*. The union of these sets is
the privilege representation of the actual rule `r2` in the log.

Sampling of the privilege of a corresponding pair of rules is not fully uniform.
1. The set of actions applicable to a rule is sampled uniformly at random.
2. The set of principles *capable of performing that action on some resource* is
   sampled uniformly at random.
3. The set of resources upon which the principal can apply that action is
   selected uniformly at random.

So, for example if a rule is applicable to actions `A1` and `A2`, but the
partition of the privilege of the rule corresponding to `A2` is very small, that
partition will likely be over-represented in the privilege representation of the
rule in the log. The above also assumes that each step can succeed, which is a
guarantee that entity generation must ensure.

### Data Family Generation

The generation of families of datasets, like with the generation of a particular
dataset, is driven by the entities designated as primary for the case study. For
two family members of sizes `n1` and `n2` where `n1 < n2`
- the number of new entities of a given primary entity type is calculated by
  `n2 * ENTITY_SIZE_RATIO - n1 * ENTITY_SIZE_RATIO` (where `ENTITY_SIZE_RATIO`
  is the value of the command line argument associated with that entity type)
- the number of new entities of a given secondary entity type is calculated
  randomly but in proportion to the number of newly-added primary and secondary
  entities (if applicable)
  - For the GClassroom example: new courses are generated randomly in proportion
    to the number of new teachers, and new assignments are generated randomly in
    proportion to the number of new courses.
- extension of the primary entity relations is driven solely on the basis of
  the newly-added primary and secondary entities
  - For the GClassroom example:
      - when associating courses with teachers, only new teachers are
        considered as instructors for new courses.
      - when associating new assignments, only new courses are considered.
- extension of the secondary entity relations is in proportion to the
  newly-added primary entities and the *cumulative* secondary/tertiary entities.
  - For the GClassroom example:
    - newly-created students and TAs are assigned courses randomly from the pool
      of new and old courses.
- new tertiary entities are generated based on the newly-added primary entities
  and the cumulative secondary/tertiary entities
  For example:
  - for GClassroom:
    - for each newly-added student, we gather all the assignments for all the
      courses in which the student is enrolled (which may be new or old courses)
      and generate a new grade entity for each distinct pair.

Internally, the machinery for data families distinguishes between the "additive"
and "cumulative" representations of the datasets with the aliases `Family` and
`Pools`, resp. The entity and log generators produce a `Family` that is
converted to `Pools` before writing to file (so, each generated .json file
contains a cumulative dataset containing all the distinct entries from the
smaller datasets generated from the same run).

The generation of log families is fairly straightforward: for each dataset
family member, and for each policy rule, a principal is chosen randomly from the
set of entities newly introduced in the current family member, with the
resources drawn randomly from all entities generated so far.

### Invocation

Executable through stack.

Run

    stack exec cedar-restrict-gclassroom-exe -- --help

for usage instructions. For example, for the default GClassroom case study,

    stack exec cedar-restrict-gclassroom-exe -- gc


## Setup

Built using stack version 2.15.7. We recommend using ghcup, and letting it manage stack.

- `stack update` to refresh package
- `stack build` to build the case study generator
- `stack exec -- cedar-restrict-case-study --help` for help invoking the generator
  Example execution

    `stack exec -- cedar-restrict-case-study gc --size-start=10 --size-incr=10 --size-end=60`

  This can take some time, especially for large size parameters and a large number of datasets
## Directory

- Entity store generation is performed in `app/${CASE_STUDY}/GenEntities.hs`
- Log generation is performed in `app/${CASE_STUDY}/GenLogs.hs`
- The Cedar schema file is `assets/${CASE_STUDY}/schema.cedarschema`
- The Cedar policy file is `assets/${CASE_STUDY}/policies.cedar`

## Conventions

- In the `GenLogs.hs` files: the "p" prefix indicates a function that produces a
  request that should be allowed ("p"ermited) in both the original and tightened
  policy; the "eop" prefix indicates a function producing a request that should
  be allowed by the original policy, but not in the tightened policy
  ("e"xercised "o"ver"p"rivilege)
