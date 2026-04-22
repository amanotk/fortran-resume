#!/bin/bash
# Usage: ./eval/check_assignment2.sh <student_id> <work_dir> <source_file>
# Returns: 0 if PASS, 1 if FAIL

set -e

STUDENT_ID="$1"
WORK_DIR="$2"
SOURCE_FILE="$3"

# Check arguments
if [ -z "${STUDENT_ID}" ] || [ -z "${WORK_DIR}" ] || [ -z "${SOURCE_FILE}" ]; then
    echo "Usage: $0 <student_id> <work_dir> <source_file>"
    exit 1
fi

echo "=== Evaluating Assignment 2 ==="
echo "Student ID: ${STUDENT_ID}"
echo "Source: ${SOURCE_FILE}"
echo ""

# 1. Compile original source
echo "[1/5] Compiling..."
if ! gfortran -o "${WORK_DIR}/a.out" "${SOURCE_FILE}"; then
    echo "COMPILE: FAIL"
    echo "Result: FAIL"
    exit 1
fi
echo "COMPILE: SUCCESS"

# 2. Set power = 4 for consistent evaluation
echo "[2/5] Setting power = 4..."
if grep -q "integer.*parameter.*::.*power" "${SOURCE_FILE}"; then
    sed -i 's/integer.*parameter.*::.*power.*=.*/integer, parameter :: power = 4/' "${SOURCE_FILE}"
    # Recompile with modified power
    if ! gfortran -o "${WORK_DIR}/a.out" "${SOURCE_FILE}"; then
        echo "COMPILE: FAIL (after power modification)"
        echo "Result: FAIL"
        exit 1
    fi
    echo "Power set to 4"
else
    echo "WARNING: Could not find power parameter, using original"
fi

# 3. Run test with timeout
echo "[3/5] Running test..."
OUTPUT=$(timeout 30 "${WORK_DIR}/a.out" 2>&1) || EXIT_CODE=$?
if [ "${EXIT_CODE:-0}" -eq 124 ]; then
    echo "TIMEOUT: Execution exceeded 30 seconds"
    echo "Result: FAIL"
    exit 1
fi
echo "${OUTPUT}"

# 4. Check correctness
echo "[4/5] Checking correctness..."
CORRECT=1
# Check if all "checking" lines end with "done" (regardless of sort name)
if echo "${OUTPUT}" | grep "^checking" | grep -qv "done$"; then
    echo "WARNING: Some checks did not complete"
    CORRECT=0
fi
if ! echo "${OUTPUT}" | grep -q "^checking"; then
    echo "WARNING: No check output found"
    CORRECT=0
fi

if [ "${CORRECT}" -eq 1 ]; then
    echo "CORRECTNESS: PASS"
else
    echo "CORRECTNESS: FAIL"
    RESULT="FAIL"
    cat > "${WORK_DIR}/assignment2_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 2 (assignment2)
Compile: SUCCESS
Correctness: FAIL
Result: ${RESULT}
EOF
    echo ""
    echo "Result: ${RESULT}"
    echo "Results saved to: ${WORK_DIR}/assignment2_results.txt"
    exit 1
fi

# 5. Check output format and performance
echo "[5/5] Checking output format..."
if ! echo "${OUTPUT}" | grep -q "# data size"; then
    echo "OUTPUT FORMAT: FAIL (missing table header)"
    RESULT="FAIL"
    cat > "${WORK_DIR}/assignment2_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 2 (assignment2)
Compile: SUCCESS
Correctness: PASS
Output Format: FAIL
Result: ${RESULT}
EOF
    echo ""
    echo "Result: ${RESULT}"
    echo "Results saved to: ${WORK_DIR}/assignment2_results.txt"
    exit 1
fi

# Count data rows (lines with numeric data size)
DATA_ROWS=$(echo "${OUTPUT}" | grep -E "^\s+[0-9]+" | wc -l)
if [ "${DATA_ROWS}" -lt 4 ]; then
    echo "OUTPUT FORMAT: WARNING (only ${DATA_ROWS} data rows, expected >= 4)"
else
    echo "OUTPUT FORMAT: PASS (${DATA_ROWS} data rows)"
fi

# Performance check: compare my_sort vs bubble_sort at largest N
echo "Checking performance..."
LAST_ROW=$(echo "${OUTPUT}" | grep -E "^\s+[0-9]+" | tail -n 1)
if [ -n "${LAST_ROW}" ]; then
    # Parse the last row: data_size bubble_sort_time my_sort_time
    BUBBLE_TIME=$(echo "${LAST_ROW}" | awk '{print $2}')
    MY_TIME=$(echo "${LAST_ROW}" | awk '{print $3}')
    
    if [ -n "${BUBBLE_TIME}" ] && [ -n "${MY_TIME}" ]; then
        # Convert to comparable format using awk
        PERFORMANCE_CHECK=$(awk -v bubble="${BUBBLE_TIME}" -v my="${MY_TIME}" 'BEGIN {
            if (my >= bubble) {
                print "WARNING"
            } else {
                print "PASS"
            }
        }')
        
        if [ "${PERFORMANCE_CHECK}" = "WARNING" ]; then
            echo "PERFORMANCE: WARNING (my_sort not faster than bubble_sort at largest N)"
            PERF_STATUS="WARNING"
        else
            echo "PERFORMANCE: PASS"
            PERF_STATUS="PASS"
        fi
    else
        echo "PERFORMANCE: SKIP (could not parse times)"
        PERF_STATUS="SKIP"
    fi
else
    echo "PERFORMANCE: SKIP (no data rows found)"
    PERF_STATUS="SKIP"
fi

RESULT="PASS"

# Write results
cat > "${WORK_DIR}/assignment2_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 2 (assignment2)
Compile: SUCCESS
Correctness: PASS
Output Format: PASS
Performance: ${PERF_STATUS:-SKIP}
Result: ${RESULT}
EOF

echo ""
echo "Result: ${RESULT}"
echo "Results saved to: ${WORK_DIR}/assignment2_results.txt"

exit 0
