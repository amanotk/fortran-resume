#!/bin/bash
# Usage: ./eval/check_kadai4.sh <student_id> <work_dir> <source_file>
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

echo "=== Evaluating Report 4 (kadai4) ==="
echo "Student ID: ${STUDENT_ID}"
echo "Source: ${SOURCE_FILE}"
echo ""

# TODO: Implement kadai4 evaluation
# - Multi-precision pi calculation
# - Verify pi digits

echo "[TODO] Implement kadai4 evaluation"

RESULT="PENDING"

# Write results
cat > "${WORK_DIR}/kadai4_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 4 (kadai4)
Result: ${RESULT}
EOF

echo ""
echo "Result: ${RESULT}"
echo "Results saved to: ${WORK_DIR}/kadai4_results.txt"

exit 0
