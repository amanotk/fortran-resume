#!/bin/bash
# Usage: ./eval/evaluate.sh <student_id>

set -e

STUDENT_ID="$1"
WORK_DIR="eval/work/${STUDENT_ID}"

# Check argument
if [ -z "${STUDENT_ID}" ]; then
    echo "Usage: $0 <student_id>"
    exit 1
fi

# 1. Create working directory
mkdir -p "${WORK_DIR}"

# 2. Download submissions
echo "=== Downloading submissions ==="
uv run python eval/download.py "${STUDENT_ID}" "${WORK_DIR}"

# 3. Find Report 1 file
SOURCE_FILE=$(find "${WORK_DIR}" -name '*kadai1*.f90' -type f | head -n 1)
if [ -z "${SOURCE_FILE}" ]; then
    echo "ERROR: No kadai1 submission found"
    exit 1
fi

echo "Found: ${SOURCE_FILE}"

# 4. Compile
echo "=== Compiling ==="
gfortran -o "${WORK_DIR}/a.out" "${SOURCE_FILE}"

# 5. Run test
echo "=== Running test ==="
OUTPUT=$(echo "1 180" | "${WORK_DIR}/a.out")
echo "${OUTPUT}"

# 6. Basic check (just verify output contains expected keywords)
if echo "${OUTPUT}" | grep -q "Area" && echo "${OUTPUT}" | grep -q "Length"; then
    RESULT="PASS"
else
    RESULT="FAIL"
fi

# 7. Write results
cat > "${WORK_DIR}/results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 1
Result: ${RESULT}
EOF

echo ""
echo "=== Evaluation Complete ==="
echo "Result: ${RESULT}"
echo "Results saved to: ${WORK_DIR}/results.txt"
