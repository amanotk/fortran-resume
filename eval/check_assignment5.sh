#!/bin/bash
# Usage: ./eval/check_assignment5.sh <student_id> <work_dir> <source_dir>
# Returns: 0 if PASS, 1 if FAIL

set -e

STUDENT_ID="$1"
WORK_DIR="$2"
SOURCE_DIR="$3"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEMPLATE_DIR="${SCRIPT_DIR}/../assignment/template/assignment5"

# Check arguments
if [ -z "${STUDENT_ID}" ] || [ -z "${WORK_DIR}" ] || [ -z "${SOURCE_DIR}" ]; then
    echo "Usage: $0 <student_id> <work_dir> <source_dir>"
    exit 1
fi

echo "=== Evaluating Assignment 5 ==="
echo "Student ID: ${STUDENT_ID}"
echo "Source: ${SOURCE_DIR}"
echo ""

# Step 1: Detect .f90 files
echo "[1/4] Detecting files..."
mapfile -t F90_FILES < <(find "${SOURCE_DIR}" -maxdepth 1 -name "*.f90" -type f)
FILE_COUNT=${#F90_FILES[@]}

if [ "${FILE_COUNT}" -eq 0 ]; then
    echo "ERROR: No .f90 files found in ${SOURCE_DIR}"
    echo "Result: FAIL"
    exit 1
fi

if [ "${FILE_COUNT}" -gt 2 ]; then
    echo "ERROR: Too many .f90 files found (${FILE_COUNT}). Expected 1 or 2."
    echo "Result: FAIL"
    exit 1
fi

# Step 2: Identify and copy files
DIMENSIONS_FILE=""
MAIN_FILE=""

if [ "${FILE_COUNT}" -eq 1 ]; then
    # Single file: assume it's dimensions.f90
    echo "Found 1 file(s): $(basename "${F90_FILES[0]}")"
    echo "Using template main.f90 for testing"
    DIMENSIONS_FILE="${F90_FILES[0]}"
    
    # Copy student's dimensions.f90 (skip if same file)
    if [ "$(realpath "${DIMENSIONS_FILE}")" != "$(realpath "${WORK_DIR}/dimensions.f90" 2>/dev/null)" ]; then
        cp -f "${DIMENSIONS_FILE}" "${WORK_DIR}/dimensions.f90"
    fi
    
    # Copy template's main.f90
    if [ ! -f "${TEMPLATE_DIR}/main.f90" ]; then
        echo "ERROR: Template main.f90 not found at ${TEMPLATE_DIR}/main.f90"
        echo "Result: FAIL"
        exit 1
    fi
    cp -f "${TEMPLATE_DIR}/main.f90" "${WORK_DIR}/main.f90"
    
elif [ "${FILE_COUNT}" -eq 2 ]; then
    # Two files: identify by content
    echo "Found 2 file(s)"
    
    for FILE in "${F90_FILES[@]}"; do
        if grep -q "module dimensions" "${FILE}"; then
            DIMENSIONS_FILE="${FILE}"
        elif grep -q "program test_dimensions" "${FILE}"; then
            MAIN_FILE="${FILE}"
        fi
    done
    
    # Fallback to filename matching if content detection incomplete
    if [ -z "${DIMENSIONS_FILE}" ] || [ -z "${MAIN_FILE}" ]; then
        for FILE in "${F90_FILES[@]}"; do
            BASENAME=$(basename "${FILE}")
            if [ -z "${DIMENSIONS_FILE}" ] && echo "${BASENAME}" | grep -qi "dimension"; then
                DIMENSIONS_FILE="${FILE}"
            fi
            if [ -z "${MAIN_FILE}" ] && echo "${BASENAME}" | grep -qiE "main|test"; then
                MAIN_FILE="${FILE}"
            fi
        done
    fi
    
    # Validate identification
    if [ -z "${DIMENSIONS_FILE}" ]; then
        echo "ERROR: Could not identify dimensions.f90 (missing 'module dimensions')"
        echo "Result: FAIL"
        exit 1
    fi
    
    if [ -z "${MAIN_FILE}" ]; then
        echo "ERROR: Could not identify main.f90 (missing 'program test_dimensions')"
        echo "Result: FAIL"
        exit 1
    fi
    
    # Check for duplicate modules
    MODULE_COUNT=0
    for FILE in "${F90_FILES[@]}"; do
        if grep -q "module dimensions" "${FILE}"; then
            MODULE_COUNT=$((MODULE_COUNT + 1))
        fi
    done
    if [ "${MODULE_COUNT}" -gt 1 ]; then
        echo "ERROR: Multiple files contain 'module dimensions'"
        echo "Result: FAIL"
        exit 1
    fi
    
    echo "  dimensions.f90: $(basename "${DIMENSIONS_FILE}")"
    echo "  main.f90: $(basename "${MAIN_FILE}")"
    
    # Copy both files (skip if same file)
    if [ "$(realpath "${DIMENSIONS_FILE}")" != "$(realpath "${WORK_DIR}/dimensions.f90" 2>/dev/null)" ]; then
        cp -f "${DIMENSIONS_FILE}" "${WORK_DIR}/dimensions.f90"
    fi
    if [ "$(realpath "${MAIN_FILE}")" != "$(realpath "${WORK_DIR}/main.f90" 2>/dev/null)" ]; then
        cp -f "${MAIN_FILE}" "${WORK_DIR}/main.f90"
    fi
fi

echo ""

# Step 3: Compile
echo "[2/4] Compiling..."
cd "${WORK_DIR}"
if ! gfortran -O2 -Wall dimensions.f90 main.f90 -o test_dimensions 2>&1; then
    echo "COMPILE: FAIL"
    cat > "${WORK_DIR}/assignment5_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 5 (assignment5)
Files Submitted: ${FILE_COUNT}
Compile: FAIL
Result: FAIL
EOF
    echo ""
    echo "Result: FAIL"
    echo "Results saved to: ${WORK_DIR}/assignment5_results.txt"
    exit 1
fi
echo "COMPILE: SUCCESS"
echo ""

# Step 4: Run tests
echo "[3/4] Running tests..."
OUTPUT=$(timeout 30 ./test_dimensions 2>&1) || EXIT_CODE=$?

if [ "${EXIT_CODE:-0}" -eq 124 ]; then
    echo "TIMEOUT: Execution exceeded 30 seconds"
    cat > "${WORK_DIR}/assignment5_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 5 (assignment5)
Files Submitted: ${FILE_COUNT}
Compile: SUCCESS
Result: FAIL (timeout)
EOF
    echo ""
    echo "Result: FAIL"
    echo "Results saved to: ${WORK_DIR}/assignment5_results.txt"
    exit 1
fi

echo "Execution completed"
echo ""

# Step 5: Check results
echo "[4/4] Checking results..."

# Extract summary: "Results: X passed, Y failed" (may span multiple lines)
SUMMARY_LINE=$(echo "${OUTPUT}" | tr '\n' ' ' | grep -oP 'Results:.*?failed' | tail -1)
if [ -z "${SUMMARY_LINE}" ]; then
    echo "WARNING: Could not find test summary in output"
    PASSED=0
    FAILED=0
else
    PASSED=$(echo "${SUMMARY_LINE}" | grep -oP '\K[0-9]+(?=\s+passed)' || echo "0")
    FAILED=$(echo "${SUMMARY_LINE}" | grep -oP '\K[0-9]+(?=\s+failed)' || echo "0")
fi

echo "Tests Passed: ${PASSED:-0}"
echo "Tests Failed: ${FAILED:-0}"

# Check for advanced problem (発展課題) implementation
echo ""
echo "[Bonus] Checking advanced features (SI prefixes)..."
ADVANCED_FEATURES=""
ADVANCED_COUNT=0

# Check for SI prefix handling - look for explicit prefix parsing logic
# Check for kilo (k)
if grep -qE 'case\s*\(\s*"km?"?\s*\)|prefix.*[kK].*1000|scale.*1\.0e3' "${WORK_DIR}/dimensions.f90"; then
    ADVANCED_FEATURES="${ADVANCED_FEATURES}kilo (k), "
    ADVANCED_COUNT=$((ADVANCED_COUNT + 1))
fi

# Check for mega (M) - be careful not to match MASS_DIM
if grep -qE 'case\s*\(\s*"M[gs]?"?\s*\)|prefix.*[Mm]ega.*1\.0e6|scale.*1\.0e6' "${WORK_DIR}/dimensions.f90"; then
    ADVANCED_FEATURES="${ADVANCED_FEATURES}mega (M), "
    ADVANCED_COUNT=$((ADVANCED_COUNT + 1))
fi

# Check for milli (m) - look for millisecond or millimeter specifically
if grep -qE 'case\s*\(\s*"ms"\s*\)' "${WORK_DIR}/dimensions.f90" || grep -qE 'case\s*\(\s*"mm"\s*\)' "${WORK_DIR}/dimensions.f90" || grep -qi 'milli' "${WORK_DIR}/dimensions.f90"; then
    ADVANCED_FEATURES="${ADVANCED_FEATURES}milli (m), "
    ADVANCED_COUNT=$((ADVANCED_COUNT + 1))
fi

# Check for micro (u)
if grep -qE 'case\s*\(\s*"u[ms]?"?\s*\)|prefix.*micro.*1\.0e-6|scale.*1\.0e-6' "${WORK_DIR}/dimensions.f90"; then
    ADVANCED_FEATURES="${ADVANCED_FEATURES}micro (u), "
    ADVANCED_COUNT=$((ADVANCED_COUNT + 1))
fi

# Check for general prefix parsing logic
if grep -qE 'prefix.*select\s+case|parse.*prefix' "${WORK_DIR}/dimensions.f90"; then
    ADVANCED_FEATURES="${ADVANCED_FEATURES}prefix parsing, "
    ADVANCED_COUNT=$((ADVANCED_COUNT + 1))
fi

# Remove trailing comma and space
ADVANCED_FEATURES=$(echo "${ADVANCED_FEATURES}" | sed 's/, $//')

if [ "${ADVANCED_COUNT}" -gt 0 ]; then
    echo "Advanced Features Detected: ${ADVANCED_FEATURES}"
    echo "Development Status: (1) 基本課題 + (2) 発展課題 (partial)"
else
    echo "Advanced Features: Not detected"
    echo "Development Status: (1) 基本課題 only"
fi

# Determine final result
if [ "${FAILED:-0}" -eq 0 ] && [ "${PASSED:-0}" -gt 0 ]; then
    RESULT="PASS"
    FINAL_EXIT=0
else
    RESULT="FAIL"
    FINAL_EXIT=1
fi

# Save test output
echo "${OUTPUT}" > "${WORK_DIR}/assignment5_output.txt"
echo "Output saved to: ${WORK_DIR}/assignment5_output.txt"

# Write results
cat > "${WORK_DIR}/assignment5_results.txt" << EOF
Student ID: ${STUDENT_ID}
Report: 5 (assignment5)
Files Submitted: ${FILE_COUNT}
Compile: SUCCESS
Tests Passed: ${PASSED:-0}
Tests Failed: ${FAILED:-0}
Advanced Features: ${ADVANCED_COUNT:-0} detected
Development Status: $([ "${ADVANCED_COUNT:-0}" -gt 0 ] && echo "(1) 基本課題 + (2) 発展課題 (partial)" || echo "(1) 基本課題 only")
Result: ${RESULT}
EOF

echo ""
echo "Result: ${RESULT}"
echo "Results saved to: ${WORK_DIR}/assignment5_results.txt"

exit ${FINAL_EXIT}
