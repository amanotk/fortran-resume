#!/bin/bash
# Usage: ./eval/check_kadai1.sh <student_id> <work_dir> <source_file>
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

echo "=== Evaluating Report 1 (kadai1) ==="
echo "Student ID: ${STUDENT_ID}"
echo "Source: ${SOURCE_FILE}"
echo ""

# 1. Compile
echo "[1/3] Compiling..."
if ! gfortran -o "${WORK_DIR}/a.out" "${SOURCE_FILE}"; then
    echo "COMPILE: FAIL"
    echo "Result: FAIL"
    exit 1
fi
echo "COMPILE: SUCCESS"

# 2. Run test
echo "[2/3] Running test..."
OUTPUT=$(echo "1 180" | "${WORK_DIR}/a.out" 2>&1) || true
echo "${OUTPUT}"

# 3. Check output
echo "[3/3] Checking output..."
if echo "${OUTPUT}" | grep -q "Area" && echo "${OUTPUT}" | grep -q "Length"; then
    echo "OUTPUT: PASS"
    RESULT="PASS"
else
    echo "OUTPUT: FAIL"
    RESULT="FAIL"
fi

# Write detailed results
cat > "${WORK_DIR}/kadai1_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 1 (kadai1)
Compile: SUCCESS
Run: SUCCESS
Output Check: ${RESULT}
Result: ${RESULT}
EOF

echo ""
echo "Result: ${RESULT}"
echo "Results saved to: ${WORK_DIR}/kadai1_results.txt"

# Return appropriate exit code
if [ "${RESULT}" = "PASS" ]; then
    exit 0
else
    exit 1
fi
