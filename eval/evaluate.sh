#!/bin/bash
# Usage: ./eval/evaluate.sh <student_id> [assignment_num]
# If assignment_num is specified, only evaluate that assignment (1-5)
# Otherwise, evaluate all assignments found

set -e

STUDENT_ID="$1"
ASSIGNMENT_NUM="$2"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORK_DIR="${SCRIPT_DIR}/work/${STUDENT_ID}"

# Load environment variables from .env if it exists
if [ -f "${SCRIPT_DIR}/../.env" ]; then
    set -a
    # shellcheck source=/dev/null
    . "${SCRIPT_DIR}/../.env"
    set +a
fi

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
if [ -n "${ASSIGNMENT_NUM}" ]; then
    # Download specific assignment
    uv run python "${SCRIPT_DIR}/download.py" --force --assignment "${ASSIGNMENT_NUM}" "${STUDENT_ID}" "${WORK_DIR}"
else
    # Download latest submission (all assignments)
    uv run python "${SCRIPT_DIR}/download.py" --force "${STUDENT_ID}" "${WORK_DIR}"
fi
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
            # Find all assignment5 files (student may submit 1 or 2 files)
            mapfile -t SOURCE_FILES < <(find "${WORK_DIR}" -maxdepth 1 -name '*assignment5*.f90' -type f)
            if [ ${#SOURCE_FILES[@]} -eq 0 ]; then
                echo "ERROR: No assignment5 submission found"
                exit 1
            fi
            echo "Found ${#SOURCE_FILES[@]} file(s):"
            for f in "${SOURCE_FILES[@]}"; do
                echo "  - $(basename "$f")"
            done
            echo ""
            # Create temp directory with all assignment5 files
            A5_DIR="${WORK_DIR}/assignment5_submission"
            rm -rf "${A5_DIR}" && mkdir -p "${A5_DIR}"
            cp "${SOURCE_FILES[@]}" "${A5_DIR}/"
            "${SCRIPT_DIR}/check_assignment5.sh" "${STUDENT_ID}" "${WORK_DIR}" "${A5_DIR}"
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
    mapfile -t SOURCE_FILES < <(find "${WORK_DIR}" -maxdepth 1 -name '*assignment5*.f90' -type f)
    if [ ${#SOURCE_FILES[@]} -gt 0 ]; then
        echo "=== Assignment 5 ==="
        echo "Found ${#SOURCE_FILES[@]} file(s):"
        for f in "${SOURCE_FILES[@]}"; do
            echo "  - $(basename "$f")"
        done
        echo ""
        # Create temp directory with all assignment5 files
        A5_DIR="${WORK_DIR}/assignment5_submission"
        rm -rf "${A5_DIR}" && mkdir -p "${A5_DIR}"
        cp "${SOURCE_FILES[@]}" "${A5_DIR}/"
        "${SCRIPT_DIR}/check_assignment5.sh" "${STUDENT_ID}" "${WORK_DIR}" "${A5_DIR}" || true
        echo ""
    fi
    
    # Summary
    echo "=== Evaluation Summary ==="
    echo "Results saved to: ${WORK_DIR}/"
    ls -la "${WORK_DIR}"/*_results.txt 2>/dev/null || echo "No results found"
fi
