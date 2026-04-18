---
name: run-evaluation
description: Evaluate student assignment submissions and review code against requirements (reports in Japanese)
---

# Student Assignment Evaluation Skill

## Usage

```
@run-evaluation <student_id> <assignment_num>
```

- `student_id`: Required. Student ID (e.g., `05262601`)
- `assignment_num`: Required. Assignment number 1-5

## Workflow

### Step 1: Run Evaluation Script

```bash
./eval/evaluate.sh <student_id> <assignment_num>
```

### Step 2: Check Results

If submission not found or compilation fails, stop with error message.

### Step 3: Read Assignment Requirements

Read `assignment/assignment<N>.qmd` to understand requirements.

### Step 4: Read Student Code

Read `eval/work/<student_id>/*assignment<N>*.f90` for submitted code.

### Step 5: Code Review

Compare code against requirements and provide review in **Japanese**.

### Step 6: Overall Score

Provide overall score out of **10**.

---

## Output Format (in Japanese)

```markdown
=== 評価結果 ===
学生 ID: <student_id>
課題：<assignment_num>

【自動テスト結果】
コンパイル：<成功/失敗>
実行：<成功/失敗>
出力チェック：<PASS/FAIL>

【コードレビュー】
✓ 要件 1: <説明>
✓ 要件 2: <説明>
...

【Fortran スタイル】
✓/✗ implicit none 使用
✓/✗ 変数名：分かりやすい
✓/✗ インデント：2 スペース

【コード例】
```fortran
<短いコード引用>
```

【総合評価】X/10

【改善点】
- <具体的な改善提案>
```

---

## Assignment-Specific Criteria

### Assignment 1: Fan Calculator

**Requirements**:
- [ ] Read radius `r` and angle `θ` (degrees) from stdin
- [ ] Calculate fan area: `r²θπ/360`
- [ ] Calculate arc length: `rθπ/180`
- [ ] Output format: `Area of fan:`, `Length of arc:`

**Check Points**:
- Input: `read(*, *) r, theta`
- Pi calculation: `pi = 4.0_8 * atan(1.0_8)` or similar
- Formula accuracy
- Output format matches spec

### Assignment 2: Sort Algorithm

**Requirements**:
- [ ] Implement quick sort, merge sort, or heap sort (not bubble sort)
- [ ] Pass sort verification
- [ ] Output performance comparison

**Check Points**:
- Not bubble sort
- Recursive/iterative correctness
- Time complexity matches theory
- `my_sort` subroutine implementation

### Assignment 3: Hilbert Curve

**Requirements**:
- [ ] Use recursive subroutine/function
- [ ] Output coordinate sequence to file
- [ ] gnuplot-plotable format

**Check Points**:
- Recursive implementation correctness
- LDR, URD, RUL, DLU rotations
- Coordinate update logic
- Output format

### Assignment 4: Multi-precision Pi

**Requirements**:
- [ ] Implement multi-digit addition, subtraction, division
- [ ] Calculate π using Machin's formula
- [ ] Correct digits for specified precision

**Check Points**:
- Multi-digit number representation
- Pen-and-paper arithmetic algorithm
- Digit accuracy
- Output format (10-digit groups)

### Assignment 5: Physical Quantity Module

**Requirements**:
- [ ] `quantity_t` type (value + dimension array)
- [ ] `quantity(val, unit_str)` function
- [ ] Operators: `+`, `-`, `*`, `/`
- [ ] Dimension mismatch handling (return NaN)
- [ ] Pass test suite

**Check Points**:
- Unit string parsing (`m`, `kg`, `s`, `m/s`, `m^2`, etc.)
- Dimension array handling `[length, mass, time]`
- Operator overloading
- Error handling

---

## Fortran Style Checklist

- [ ] Uses `implicit none`
- [ ] Uses `intent(inout)` appropriately
- [ ] 2-space indentation
- [ ] Clear variable names
- [ ] Appropriate subroutine/function names

---

## Scoring Rubric (out of 10)

| Score | Criteria |
|-------|----------|
| **9-10** | Complete implementation, perfect style, almost no improvements needed |
| **7-8** | Functionally complete, minor issues |
| **5-6** | Main features OK, room for improvement |
| **3-4** | Partial implementation, major improvements needed |
| **1-2** | Compiles only, significant functionality missing |
| **0** | No submission / compilation failure |

---

## Important Notes

- **Stop if not found**: If submission not found, show error and stop
- **No cleanup**: Do not clean up `eval/work/` directory
- **Ignore templates**: Do not reference `assignment/template/` files
- **Concise code quotes**: Keep code examples short and focused
- **Report in Japanese**: All output must be in Japanese (technical terms can be in English)
