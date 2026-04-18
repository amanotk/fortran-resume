# AGENTS.md - Development Guidelines

Fortran exercise materials for University of Tokyo "Earth and Planetary Physics Exercise".

## Build Commands

```bash
uv sync                    # Install dependencies (Python 3.10-3.12)
uv run make html           # Build HTML docs to _build/html/
uv run make clean          # Clean build artifacts
quarto render assignment   # Build assignment PDFs
./eval/evaluate.sh <student_id> [assignment_num]  # Evaluate submissions
gfortran -o a.out source.f90    # Compile Fortran
```

## Python Style

- **Imports**: stdlib → third-party → local; use `from pathlib import Path`
- **Formatting**: PEP 8, type hints required, Google-style docstrings
- **Error handling**: Specific exceptions, `ValueError` for invalid args, `sys.exit(1)` for CLI
- **Naming**: `snake_case` for functions/variables, `UPPER_CASE` for constants
- **Type hints**:
  ```python
  def extract_sheet_id(url: str) -> str:
      """Extract sheet ID from Google Sheets URL."""
  ```

## Fortran Style

- **Formatting**: 2-space indent (`.fprettify.rc`), run `fprettify source.f90`
- **Structure**: `implicit none` everywhere, `intent(inout)` for arguments
- **Naming**: `snake_case`, descriptive names (`calculate_area()` not `calc()`)

## Shell Script Style

- `#!/bin/bash`, quote variables `"${VAR}"`, `set -e` for exit-on-error
- Error handling:
  ```bash
  if ! command; then echo "ERROR: message"; exit 1; fi
  ```

## Directory Structure

```
assignment/   # QMD files and templates    data/         # Sample data files
docs/         # Sphinx docs source         eval/         # Evaluation scripts
sample/       # Sample code by chapter     answer/       # Answer keys
_build/       # Build output (gitignored)
```

## Git & CI/CD

- Commits: imperative mood ("Add feature"), atomic commits
- Push to `main` → GitHub Pages deploy (Python 3.11, Quarto 1.6.40)

## Testing

```bash
uv run python eval/download.py <student_id> <output_dir>
./eval/evaluate.sh test123 1
./eval/check_assignment1.sh <student_id> <work_dir> <source_file>
```

## Notes

- `eval/work/` and `test/` are gitignored
- Python 3.10-3.12 required
- `SHEET_URL` env var configurable
- Fonts: `assignment/fonts/` for Japanese
