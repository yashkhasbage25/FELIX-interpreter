#!/bin/sh
cabal build
# no point if build fails
if [[ $? -ne 0 ]]; then
    echo "BUILD FAILED. Testing aborted"
    exit 1
fi

# all test files
declare -a FileNameArray=("env" "miscellaneous" "gcd" "ifelse" "loop" "loop" "pattern" "power" "scope" "whileloop")

TEST="test"
# check if test directory exists
if [ -d "./$TEST" ]; then
    echo "Found directory test."
else
    echo "Test directory not found."
    echo "Testing aborted."
    exit 1
fi

# test output dirs
TESTANS="./$TEST/testanswers"
TESTOUT="./$TEST/testouts"
cd test
mkdir testanswers
mkdir testouts
cd ..

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

let total_test_files=0
let total_runtime_fails=0
let total_parsetime_fails=0
# filename extension

# RT is Run Time and PT is Parse time(here parser output)
FELIX_EXT=".felix"
RT_OUT_EXT=".out"
PT_OUT_EXT=".parse"
RT_ANS_EXT=".trueout"
PT_ANS_EXT=".trueparse"

# for every testfile
for filename in ${FileNameArray[@]}; do
    filepath="./$TEST/$filename$FELIX_EXT"

    # check if testfile exists
    if [[ -f $filepath ]]; then
        echo "found testfile $filepath"
    else
        echo -e "${RED}Could not find testfile $filename at $filepath${NC}"
        continue
    fi

    # testouts
    this_rt_ansfile="$TESTANS/$filename$RT_ANS_EXT"
    this_pt_ansfile="$TESTANS/$filename$PT_ANS_EXT"
    this_rt_outfile="$TESTOUT/$filename$RT_OUT_EXT"
    this_pt_outfile="$TESTOUT/$filename$PT_OUT_EXT"

    # check if answer file exists
    # runtime asnwers
    if [[ -f $this_rt_ansfile ]]; then
        echo "Found test runtime answer file $this_rt_ansfile"
    else
        echo -e "${RED}Test runtime answer file not found. Skipping the test case $filename${NC}"
        continue
    fi

    # parsetime answers
    if [[ -f $this_pt_ansfile ]]; then
        echo "Found test parsetime answer file $this_pt_ansfile"
    else
        echo -e "${RED}Test parsetime answer file not found. Skipping the test case $filename${NC}"
        continue
    fi

    let total_test_files++
    # run test file
    cabal run --verbose=0 $filepath > $this_rt_outfile
    cabal run --verbose=0 -- -P $filepath > $this_pt_outfile

    if [[ $? -ne 0 ]]; then
        echo "testfile $filename runs without errors"
    fi

    # compare output with answer
    # runtime output
    if cmp -s $this_rt_ansfile $this_rt_outfile; then
        echo -e "${YELLOW}Test Runtime for $filename Passed!${NC}"
    else
        echo -e "${RED}Test runtime for file $filename failed. ${NC}"
        let total_runtime_fails++
    fi
    # parsetime output
    if cmp -s $this_pt_ansfile $this_pt_outfile; then
        echo -e "${GREEN}Test Parsetime for $filename Passed!${NC}"
    else
        echo -e "${RED}Test parsetime for file $filename failed. ${NC}"
        let total_parsetime_fails++
    fi
done

# final stats
echo "Total number of test cases run: $total_test_files"
echo -e "Total parse time fails: ${RED}$total_parsetime_fails/$total_test_files${NC}"
echo -e "Total run time fails: ${RED}$total_runtime_fails/$total_test_files${NC}"
