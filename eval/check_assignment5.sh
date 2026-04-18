#!/bin/bash
# Usage: ./eval/check_assignment5.sh <student_id> <work_dir> <source_dir>
# Returns: 0 if PASS, 1 if FAIL

set -e

STUDENT_ID="$1"
WORK_DIR="$2"
SOURCE_DIR="$3"

# Check arguments
if [ -z "${STUDENT_ID}" ] || [ -z "${WORK_DIR}" ] || [ -z "${SOURCE_DIR}" ]; then
    echo "Usage: $0 <student_id> <work_dir> <source_dir>"
    exit 1
fi

echo "=== Evaluating Assignment 5 ==="
echo "Student ID: ${STUDENT_ID}"
echo "Source: ${SOURCE_DIR}"
echo ""

# TODO: Implement assignment5 evaluation
# - Physical quantity module with dimensions
# - Run test suite

echo "[TODO] Implement assignment5 evaluation"

RESULT="PENDING"

# Write results
cat > "${WORK_DIR}/assignment5_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 5 (assignment5)
Result: ${RESULT}
EOF

echo ""
echo "Result: ${RESULT}"
echo "Results saved to: ${WORK_DIR}/assignment5_results.txt"

exit 0
