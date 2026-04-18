#!/bin/bash
# Usage: ./eval/check_assignment4.sh <student_id> <work_dir> <source_file>
# Returns: 0 if PASS, 1 if FAIL

set -e

STUDENT_ID="$1"
WORK_DIR="$2"
SOURCE_FILE="$3"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check arguments
if [ -z "${STUDENT_ID}" ] || [ -z "${WORK_DIR}" ] || [ -z "${SOURCE_FILE}" ]; then
    echo "Usage: $0 <student_id> <work_dir> <source_file>"
    exit 1
fi

echo "=== Evaluating Assignment 4 ==="
echo "Student ID: ${STUDENT_ID}"
echo "Source: ${SOURCE_FILE}"
echo ""

# 1. Compile
echo "[1/4] Compiling..."
if ! gfortran -o "${WORK_DIR}/a.out" "${SOURCE_FILE}"; then
    echo "COMPILE: FAIL"
    echo "Result: FAIL"
    exit 1
fi
echo "COMPILE: SUCCESS"

# 2. Run with 120 digits
echo "[2/4] Running with 120 digits..."
OUTPUT=$(timeout 30 "${WORK_DIR}/a.out" 120 2>&1) || EXIT_CODE=$?
if [ "${EXIT_CODE:-0}" -eq 124 ]; then
    echo "TIMEOUT: Execution exceeded 30 seconds"
    echo "Result: FAIL"
    exit 1
fi
echo "Execution completed"

# 3. Extract digits (flexible parsing)
echo "[3/4] Extracting pi digits..."
# Remove all non-digits except first dot, extract digits after "3."
DIGITS=$(echo "${OUTPUT}" | tr -cd '0-9.' | grep -oP '3\.\K[0-9]+' | head -c 100)

if [ -z "${DIGITS}" ]; then
    echo "EXTRACTION: FAIL (no valid pi digits found)"
    RESULT="FAIL"
    cat > "${WORK_DIR}/assignment4_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 4 (assignment4)
Compile: SUCCESS
Digit Extraction: FAIL
Result: ${RESULT}
EOF
    echo ""
    echo "Result: ${RESULT}"
    echo "Results saved to: ${WORK_DIR}/assignment4_results.txt"
    exit 1
fi

DIGIT_COUNT=${#DIGITS}
echo "Extracted ${DIGIT_COUNT} digits"

if [ "${DIGIT_COUNT}" -lt 100 ]; then
    echo "EXTRACTION: FAIL (only ${DIGIT_COUNT} digits, need 100)"
    RESULT="FAIL"
    cat > "${WORK_DIR}/assignment4_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 4 (assignment4)
Compile: SUCCESS
Digit Extraction: FAIL
Digits Found: ${DIGIT_COUNT}
Result: ${RESULT}
EOF
    echo ""
    echo "Result: ${RESULT}"
    echo "Results saved to: ${WORK_DIR}/assignment4_results.txt"
    exit 1
fi

echo "EXTRACTION: PASS"

# 4. Verify digits against reference
echo "[4/4] Verifying digits..."
REFERENCE=$(cat "${SCRIPT_DIR}/pi_reference.txt")

if [ "${DIGITS}" = "${REFERENCE}" ]; then
    echo "PI DIGITS: PASS (100/100 correct)"
    DIGIT_STATUS="PASS"
    RESULT="PASS"
else
    # Find first mismatch position
    MISMATCH=0
    for i in $(seq 0 99); do
        if [ "${DIGITS:$i:1}" != "${REFERENCE:$i:1}" ]; then
            MISMATCH=$((i + 1))
            break
        fi
    done
    echo "PI DIGITS: FAIL (first mismatch at digit ${MISMATCH})"
    DIGIT_STATUS="FAIL"
    RESULT="FAIL"
fi

# Save output for review
echo "${OUTPUT}" > "${WORK_DIR}/assignment4_output.txt"
echo "Output saved to: ${WORK_DIR}/assignment4_output.txt"

# Write results
cat > "${WORK_DIR}/assignment4_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 4 (assignment4)
Compile: SUCCESS
Digit Extraction: PASS
Digits Found: ${DIGIT_COUNT}
Digit Verification: ${DIGIT_STATUS}
Result: ${RESULT}
EOF

echo ""
echo "Result: ${RESULT}"
echo "Results saved to: ${WORK_DIR}/assignment4_results.txt"

if [ "${RESULT}" = "PASS" ]; then
    exit 0
else
    exit 1
fi
