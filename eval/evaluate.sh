#!/bin/bash
# Usage: ./eval/evaluate.sh <student_id> [assignment_num]
# If assignment_num is specified, only evaluate that assignment (1-5)
# Otherwise, evaluate all assignments found

set -e

STUDENT_ID="$1"
ASSIGNMENT_NUM="$2"
WORK_DIR="eval/work/${STUDENT_ID}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check argument
if [ -z "${STUDENT_ID}" ]; then
    echo "Usage: $0 <student_id> [assignment_num]"
    echo "  assignment_num: 1-5 (optional, evaluate all if not specified)"
    exit 1
fi

# 1. Create working directory
mkdir -p "${WORK_DIR}"

# 2. Download submissions
echo "=== Downloading submissions ==="
uv run python eval/download.py "${STUDENT_ID}" "${WORK_DIR}"
echo ""

# 3. Evaluate based on assignment_num
if [ -n "${ASSIGNMENT_NUM}" ]; then
    # Evaluate specific assignment
    case "${ASSIGNMENT_NUM}" in
        1)
            SOURCE_FILE=$(find "${WORK_DIR}" -name '*assignment1*.f90' -type f | head -n 1)
            if [ -z "${SOURCE_FILE}" ]; then
                echo "ERROR: No assignment1 submission found"
                exit 1
            fi
            echo "Found: ${SOURCE_FILE}"
            echo ""
            "${SCRIPT_DIR}/check_assignment1.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}"
            ;;
        2)
            SOURCE_FILE=$(find "${WORK_DIR}" -name '*assignment2*.f90' -type f | head -n 1)
            if [ -z "${SOURCE_FILE}" ]; then
                echo "ERROR: No assignment2 submission found"
                exit 1
            fi
            echo "Found: ${SOURCE_FILE}"
            echo ""
            "${SCRIPT_DIR}/check_assignment2.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}"
            ;;
        3)
            SOURCE_FILE=$(find "${WORK_DIR}" -name '*assignment3*.f90' -type f | head -n 1)
            if [ -z "${SOURCE_FILE}" ]; then
                echo "ERROR: No assignment3 submission found"
                exit 1
            fi
            echo "Found: ${SOURCE_FILE}"
            echo ""
            "${SCRIPT_DIR}/check_assignment3.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}"
            ;;
        4)
            SOURCE_FILE=$(find "${WORK_DIR}" -name '*assignment4*.f90' -type f | head -n 1)
            if [ -z "${SOURCE_FILE}" ]; then
                echo "ERROR: No assignment4 submission found"
                exit 1
            fi
            echo "Found: ${SOURCE_FILE}"
            echo ""
            "${SCRIPT_DIR}/check_assignment4.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}"
            ;;
        5)
            SOURCE_DIR=$(find "${WORK_DIR}" -type d -name '*assignment5*' | head -n 1)
            if [ -z "${SOURCE_DIR}" ]; then
                SOURCE_DIR="${WORK_DIR}"
            fi
            "${SCRIPT_DIR}/check_assignment5.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_DIR}"
            ;;
        *)
            echo "ERROR: Invalid assignment_num: ${ASSIGNMENT_NUM} (must be 1-5)"
            exit 1
            ;;
    esac
else
    # Evaluate all assignments found
    echo "=== Evaluating all submissions ==="
    echo ""
    
    # Assignment 1
    SOURCE_FILE=$(find "${WORK_DIR}" -name '*assignment1*.f90' -type f | head -n 1)
    if [ -n "${SOURCE_FILE}" ]; then
        echo "=== Assignment 1 ==="
        "${SCRIPT_DIR}/check_assignment1.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}" || true
        echo ""
    fi
    
    # Assignment 2
    SOURCE_FILE=$(find "${WORK_DIR}" -name '*assignment2*.f90' -type f | head -n 1)
    if [ -n "${SOURCE_FILE}" ]; then
        echo "=== Assignment 2 ==="
        "${SCRIPT_DIR}/check_assignment2.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}" || true
        echo ""
    fi
    
    # Assignment 3
    SOURCE_FILE=$(find "${WORK_DIR}" -name '*assignment3*.f90' -type f | head -n 1)
    if [ -n "${SOURCE_FILE}" ]; then
        echo "=== Assignment 3 ==="
        "${SCRIPT_DIR}/check_assignment3.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}" || true
        echo ""
    fi
    
    # Assignment 4
    SOURCE_FILE=$(find "${WORK_DIR}" -name '*assignment4*.f90' -type f | head -n 1)
    if [ -n "${SOURCE_FILE}" ]; then
        echo "=== Assignment 4 ==="
        "${SCRIPT_DIR}/check_assignment4.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}" || true
        echo ""
    fi
    
    # Assignment 5
    SOURCE_DIR=$(find "${WORK_DIR}" -type d -name '*assignment5*' | head -n 1)
    if [ -n "${SOURCE_DIR}" ]; then
        echo "=== Assignment 5 ==="
        "${SCRIPT_DIR}/check_assignment5.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_DIR}" || true
        echo ""
    fi
    
    # Summary
    echo "=== Evaluation Summary ==="
    echo "Results saved to: ${WORK_DIR}/"
    ls -la "${WORK_DIR}"/*_results.txt 2>/dev/null || echo "No results found"
fi
