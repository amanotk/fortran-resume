#!/bin/bash
# Usage: ./eval/check_assignment3.sh <student_id> <work_dir> <source_file>
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

echo "=== Evaluating Assignment 3 ==="
echo "Student ID: ${STUDENT_ID}"
echo "Source: ${SOURCE_FILE}"
echo ""

# 1. Compile
echo "[1/6] Compiling..."
if ! gfortran -o "${WORK_DIR}/a.out" "${SOURCE_FILE}"; then
    echo "COMPILE: FAIL"
    echo "Result: FAIL"
    exit 1
fi
echo "COMPILE: SUCCESS"

# 2. Run with n=4
echo "[2/6] Running with n=4..."
OUTPUT=$(echo "4" | timeout 30 "${WORK_DIR}/a.out" 2>/dev/null) || EXIT_CODE=$?
if [ "${EXIT_CODE:-0}" -eq 124 ]; then
    echo "TIMEOUT: Execution exceeded 30 seconds"
    echo "Result: FAIL"
    exit 1
fi
echo "Execution completed"

# 3. Extract coordinate pairs (flexible parsing)
echo "[3/6] Checking output format..."
COORDS=$(echo "${OUTPUT}" | awk '/^[[:space:]]*-?[0-9]+\.?[0-9]*([eE][+-]?[0-9]+)?[[:space:]]+-?[0-9]+\.?[0-9]*([eE][+-]?[0-9]+)?/ {print $1, $2}')
POINT_COUNT=$(echo "${COORDS}" | grep -c . || echo 0)

if [ "${POINT_COUNT}" -eq 0 ]; then
    echo "OUTPUT FORMAT: FAIL (no valid coordinate pairs found)"
    RESULT="FAIL"
    cat > "${WORK_DIR}/assignment3_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 3 (assignment3)
Compile: SUCCESS
Output Format: FAIL
Result: ${RESULT}
EOF
    echo ""
    echo "Result: ${RESULT}"
    echo "Results saved to: ${WORK_DIR}/assignment3_results.txt"
    exit 1
fi

echo "Found ${POINT_COUNT} coordinate pairs"

# 4. Check point count (should be 4^(n+1) = 4^5 = 1024 for n=4)
EXPECTED_POINTS=1024
if [ "${POINT_COUNT}" -eq "${EXPECTED_POINTS}" ]; then
    echo "POINT COUNT: PASS (${POINT_COUNT} = 4^5)"
    POINT_COUNT_STATUS="PASS"
else
    echo "POINT COUNT: FAIL (expected ${EXPECTED_POINTS}, got ${POINT_COUNT})"
    POINT_COUNT_STATUS="FAIL"
fi

# 5. Check unique points
echo "[4/6] Checking unique points..."
UNIQUE_COUNT=$(echo "${COORDS}" | sort -u | grep -c . || echo 0)
if [ "${UNIQUE_COUNT}" -eq "${EXPECTED_POINTS}" ]; then
    echo "UNIQUE POINTS: PASS (${UNIQUE_COUNT} unique)"
    UNIQUE_STATUS="PASS"
else
    echo "UNIQUE POINTS: FAIL (expected ${EXPECTED_POINTS} unique, got ${UNIQUE_COUNT})"
    UNIQUE_STATUS="FAIL"
fi

# 5. Check coordinate range [0, 1]
echo "[5/6] Checking coordinate range..."
RANGE_CHECK=$(echo "${COORDS}" | awk '
BEGIN { valid = 1 }
{
    if ($1 < -0.001 || $1 > 1.001 || $2 < -0.001 || $2 > 1.001) {
        valid = 0
        if (NR <= 3) print "  Out of range at line", NR, ": ($1, $2)"
    }
}
END { if (valid) print "PASS"; else print "FAIL" }')

if echo "${RANGE_CHECK}" | grep -q "^PASS"; then
    echo "COORDINATE RANGE: ${RANGE_CHECK}"
    RANGE_STATUS="PASS"
else
    echo "COORDINATE RANGE: ${RANGE_CHECK}"
    RANGE_STATUS="FAIL"
fi

# 6. Check consecutive distances (informational only)
echo "[6/6] Checking consecutive distances (informational)..."
DISTANCE_INFO=$(echo "${COORDS}" | awk '
BEGIN { 
    expected = 0.03125
    count = 0
    invalid = 0
}
NR == 1 { px = $1; py = $2; next }
{
    dx = $1 - px
    dy = $2 - py
    dist = sqrt(dx*dx + dy*dy)
    if (dist > 0.05) invalid++
    px = $1; py = $2
    count++
}
END { 
    if (invalid == 0) print "All steps valid"
    else print invalid, "large steps out of", count, "(may indicate non-continuous curve)"
}')
echo "CONSECUTIVE DISTANCE: ${DISTANCE_INFO}"
DISTANCE_STATUS="INFO"

# 7. Save data for visual review
echo "Saving data for visual review..."
echo "${COORDS}" > "${WORK_DIR}/assignment3.dat"
echo "Data saved to: ${WORK_DIR}/assignment3.dat"

# Determine final result
if [ "${POINT_COUNT_STATUS}" = "PASS" ] && \
   [ "${UNIQUE_STATUS}" = "PASS" ] && \
   [ "${RANGE_STATUS}" = "PASS" ]; then
    RESULT="PASS"
else
    RESULT="FAIL"
fi

# Write results
cat > "${WORK_DIR}/assignment3_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 3 (assignment3)
Compile: SUCCESS
Point Count: ${POINT_COUNT_STATUS}
Unique Points: ${UNIQUE_STATUS}
Coordinate Range: ${RANGE_STATUS}
Distance Check: ${DISTANCE_STATUS}
Result: ${RESULT}
EOF

echo ""
echo "Result: ${RESULT}"
echo "Results saved to: ${WORK_DIR}/assignment3_results.txt"

if [ "${RESULT}" = "PASS" ]; then
    exit 0
else
    exit 1
fi
