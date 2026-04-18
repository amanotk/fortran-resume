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

【総合評価】X/10

【改善点】
- <具体的な改善提案>
```

---

## Fortran Style Checklist

- [ ] Uses `implicit none`
- [ ] Uses `intent(inout)` appropriately
- [ ] Consistent indentation
- [ ] Clear and appropriate names of variables and subroutines/functions

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
- **Report in Japanese**: All output must be in Japanese (technical terms can be in English)
- **No cleanup**: Do not clean up `eval/work/` directory
- **Concise code quotes**: Keep code examples short and focused
- **Ignore templates**: Do not reference `assignment/template/` files but if modified templates are submitted, evaluate them as normal
