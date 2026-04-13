# RESTRICTER

This is the public repository for Restricter, which is an automated tool that takes as input a Cedar policy, schema, and access log, and proposes tightened access control policy. Tightening here refers to removing unintended over-privilege from the permit rules in the input Cedar policy.

## Prerequisites
- Python 3.10 (other versions may not work with the cvc5 version)

## Installation

We interface with the cedar engine in rust using a custom python package, which are only available as pre-built wheels for x86_64 Linux and Apple Silicon Mac (tested on M4 Macbook Air) right now. We will provide the source code along with building instructions soon.

Install the provided package cedar2json using pip on a virtual environment:
```bash
python3 -m venv venv
source venv/bin/activate
pip install wheels/cedar2json-0.3.0-cp38-abi3-manylinux_2_34_x86_64.whl
```
or
```bash
python3 -m venv venv
source venv/bin/activate
pip install wheels/cedar2json-0.3.0-cp313-cp313-macosx_11_0_arm64.whl
```
for Apple Silicon Macs.


Also install the cvc5 package:
```bash
pip install cvc5==1.2.0
```

## Example Usage

An example run:
```bash
python3 restricter.py --size 60 --case-study GClassroom --log_prefix gclass_logs --entities_prefix gclass_entities --slice 3
```
will run RESTRICTER on the classroom management case study with the entity store json `gclassroom_entities.60.json`, and the log at `gclassroom_logs.60.json` (the size parameter here is 60). We run RESTRICTER on only one rule (rule 3). The other case studies and test cases are found in `cedar-restrict-cast-studies/`.


## Structure

```
.
├── README.md
├── cedar-restrict-case-studies/
├── wheels/
├── gclassroom_policies.cedar
├── gclassroom.schema.json
├── gclass_entities.60.json
├── gclass_logs.60.json
├── get_pt.py
├── parse_schema.py
├── restricter.py
├── restricter_sygus.py
└── util.py
```

`cedar-restrict-case-studies/` contains the case studies for RESTRICTER, which includes the Cedar policy and schema files, and the program for generating the entity store and logs for each case study.

`restricter.py` is the script that runs the classroom management case study, which contains the main algorithm of RESTRICTER.

`gclassroom.cedar` and `gclassroom.schema.json` are the Cedar policy and schema files for the classroom management case study.

`restricter_sygus.py` contains the main function that interfaces with SyGuS to do rule tightening.

`get_pt.py` contains the function that picks a request from the potential over-privilege set.

## Case study

Our case studies are generated using the seed `2025` to `2125` inclusive, with the size parameter from `10` to `60` with increments of `10`, and log density from `0.3` to `1.0` with increments of `0.1`. Other parameters are set to default values. 