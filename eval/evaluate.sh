#!/bin/bash
# Usage: ./eval/evaluate.sh <student_id> [kadai_num]
# If kadai_num is specified, only evaluate that kadai (1-5)
# Otherwise, evaluate all kadai found

set -e

STUDENT_ID="$1"
KADAI_NUM="$2"
WORK_DIR="eval/work/${STUDENT_ID}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check argument
if [ -z "${STUDENT_ID}" ]; then
    echo "Usage: $0 <student_id> [kadai_num]"
    echo "  kadai_num: 1-5 (optional, evaluate all if not specified)"
    exit 1
fi

# 1. Create working directory
mkdir -p "${WORK_DIR}"

# 2. Download submissions
echo "=== Downloading submissions ==="
uv run python eval/download.py "${STUDENT_ID}" "${WORK_DIR}"
echo ""

# 3. Evaluate based on kadai_num
if [ -n "${KADAI_NUM}" ]; then
    # Evaluate specific kadai
    case "${KADAI_NUM}" in
        1)
            SOURCE_FILE=$(find "${WORK_DIR}" -name '*kadai1*.f90' -type f | head -n 1)
            if [ -z "${SOURCE_FILE}" ]; then
                echo "ERROR: No kadai1 submission found"
                exit 1
            fi
            echo "Found: ${SOURCE_FILE}"
            echo ""
            "${SCRIPT_DIR}/check_kadai1.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}"
            ;;
        2)
            SOURCE_FILE=$(find "${WORK_DIR}" -name '*kadai2*.f90' -type f | head -n 1)
            if [ -z "${SOURCE_FILE}" ]; then
                echo "ERROR: No kadai2 submission found"
                exit 1
            fi
            echo "Found: ${SOURCE_FILE}"
            echo ""
            "${SCRIPT_DIR}/check_kadai2.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}"
            ;;
        3)
            SOURCE_FILE=$(find "${WORK_DIR}" -name '*kadai3*.f90' -type f | head -n 1)
            if [ -z "${SOURCE_FILE}" ]; then
                echo "ERROR: No kadai3 submission found"
                exit 1
            fi
            echo "Found: ${SOURCE_FILE}"
            echo ""
            "${SCRIPT_DIR}/check_kadai3.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}"
            ;;
        4)
            SOURCE_FILE=$(find "${WORK_DIR}" -name '*kadai4*.f90' -type f | head -n 1)
            if [ -z "${SOURCE_FILE}" ]; then
                echo "ERROR: No kadai4 submission found"
                exit 1
            fi
            echo "Found: ${SOURCE_FILE}"
            echo ""
            "${SCRIPT_DIR}/check_kadai4.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}"
            ;;
        5)
            SOURCE_DIR=$(find "${WORK_DIR}" -type d -name '*kadai5*' | head -n 1)
            if [ -z "${SOURCE_DIR}" ]; then
                SOURCE_DIR="${WORK_DIR}"
            fi
            "${SCRIPT_DIR}/check_kadai5.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_DIR}"
            ;;
        *)
            echo "ERROR: Invalid kadai_num: ${KADAI_NUM} (must be 1-5)"
            exit 1
            ;;
    esac
else
    # Evaluate all kadai found
    echo "=== Evaluating all submissions ==="
    echo ""
    
    # Kadai 1
    SOURCE_FILE=$(find "${WORK_DIR}" -name '*kadai1*.f90' -type f | head -n 1)
    if [ -n "${SOURCE_FILE}" ]; then
        echo "=== Kadai 1 ==="
        "${SCRIPT_DIR}/check_kadai1.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}" || true
        echo ""
    fi
    
    # Kadai 2
    SOURCE_FILE=$(find "${WORK_DIR}" -name '*kadai2*.f90' -type f | head -n 1)
    if [ -n "${SOURCE_FILE}" ]; then
        echo "=== Kadai 2 ==="
        "${SCRIPT_DIR}/check_kadai2.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}" || true
        echo ""
    fi
    
    # Kadai 3
    SOURCE_FILE=$(find "${WORK_DIR}" -name '*kadai3*.f90' -type f | head -n 1)
    if [ -n "${SOURCE_FILE}" ]; then
        echo "=== Kadai 3 ==="
        "${SCRIPT_DIR}/check_kadai3.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}" || true
        echo ""
    fi
    
    # Kadai 4
    SOURCE_FILE=$(find "${WORK_DIR}" -name '*kadai4*.f90' -type f | head -n 1)
    if [ -n "${SOURCE_FILE}" ]; then
        echo "=== Kadai 4 ==="
        "${SCRIPT_DIR}/check_kadai4.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_FILE}" || true
        echo ""
    fi
    
    # Kadai 5
    SOURCE_DIR=$(find "${WORK_DIR}" -type d -name '*kadai5*' | head -n 1)
    if [ -n "${SOURCE_DIR}" ]; then
        echo "=== Kadai 5 ==="
        "${SCRIPT_DIR}/check_kadai5.sh" "${STUDENT_ID}" "${WORK_DIR}" "${SOURCE_DIR}" || true
        echo ""
    fi
    
    # Summary
    echo "=== Evaluation Summary ==="
    echo "Results saved to: ${WORK_DIR}/"
    ls -la "${WORK_DIR}"/*_results.txt 2>/dev/null || echo "No results found"
fi
