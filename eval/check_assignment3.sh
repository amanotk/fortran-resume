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

# TODO: Implement assignment3 evaluation
# - Hilbert curve generator
# - Check output format (coordinate pairs)

echo "[TODO] Implement assignment3 evaluation"

RESULT="PENDING"

# Write results
cat > "${WORK_DIR}/assignment3_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 3 (assignment3)
Result: ${RESULT}
EOF

echo ""
echo "Result: ${RESULT}"
echo "Results saved to: ${WORK_DIR}/assignment3_results.txt"

exit 0
