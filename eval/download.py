#!/usr/bin/env python3
"""Download Fortran assignment submissions from Google Sheets."""

import argparse
import csv
import io
import os
import re
import sys
from pathlib import Path

import requests

SUBMISSION_SHEET_URL = os.environ.get("SUBMISSION_SHEET_URL")
if not SUBMISSION_SHEET_URL:
    print("ERROR: SUBMISSION_SHEET_URL environment variable not set", file=sys.stderr)
    sys.exit(1)


def is_html_content(content: bytes) -> bool:
    """Check if content is HTML (likely login page or error)."""
    try:
        text = content.decode("utf-8", errors="ignore").lower()
        return text.strip().startswith("<!doctype html") or text.strip().startswith(
            "<html"
        )
    except Exception:
        return False


def extract_sheet_id(url: str) -> str:
    """Extract sheet ID from Google Sheets URL."""
    match = re.search(r"/spreadsheets/d/([a-zA-Z0-9_-]+)", url)
    if not match:
        raise ValueError(f"Invalid sheet URL: {url}")
    return match.group(1)


def fetch_sheet_csv(sheet_id: str) -> list[list[str]]:
    """Fetch Google Sheet as CSV (works for public sheets)."""
    csv_url = f"https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=csv"
    response = requests.get(csv_url, timeout=30)
    response.raise_for_status()
    response.encoding = "utf-8"

    reader = csv.reader(io.StringIO(response.text))
    return list(reader)


def extract_assignment_number(kadai_text: str) -> int:
    """Extract assignment number from Japanese text (e.g., '提出課題 1' → 1)."""
    match = re.search(r"課題\s*(\d+)", kadai_text)
    if not match:
        raise ValueError(f"Cannot parse assignment number from: {kadai_text}")
    return int(match.group(1))


def parse_drive_urls(urls_text: str) -> list[str]:
    """Parse comma-separated Drive URLs into a list."""
    if not urls_text or urls_text.strip() == "":
        return []

    urls = []
    for url in urls_text.split(","):
        url = url.strip()
        if url:
            urls.append(url)
    return urls


def extract_file_id(open_url: str) -> str:
    """Extract file ID from Drive URL."""
    match = re.search(r"[?&]id=([a-zA-Z0-9_-]+)", open_url)
    if not match:
        match = re.search(r"/file/d/([a-zA-Z0-9_-]+)/", open_url)
    if not match:
        raise ValueError(f"Invalid Drive URL: {open_url}")
    return match.group(1)


def download_file(
    url: str, output_path: Path, overwrite: bool = False
) -> tuple[bool, str]:
    """Download file from Google Drive URL.

    Returns:
        Tuple of (success: bool, message: str)
    """
    if output_path.exists() and not overwrite:
        return False, "EXISTS (use --force to overwrite)"

    try:
        file_id = extract_file_id(url)

        download_urls = [
            f"https://drive.google.com/uc?export=download&id={file_id}",
            f"https://drive.google.com/uc?id={file_id}&export=download",
        ]

        for download_url in download_urls:
            response = requests.get(download_url, timeout=30, allow_redirects=True)

            if response.status_code == 200:
                content_type = response.headers.get("Content-Type", "")

                if "text/html" in content_type:
                    continue

                if is_html_content(response.content):
                    print(
                        f"  WARNING: Received HTML content (possible login page)",
                        file=sys.stderr,
                    )
                    continue

                try:
                    output_path.write_bytes(response.content)
                    return True, f"Saved ({len(response.content)} bytes)"
                except PermissionError:
                    return False, "ERROR: Permission denied"
                except OSError as e:
                    return False, f"ERROR: Cannot write file: {e}"

        return (
            False,
            "ERROR: File requires authentication or is not publicly accessible",
        )

    except requests.Timeout:
        return False, "ERROR: Download timed out"
    except Exception as e:
        return False, f"ERROR: {e}"


def main():
    parser = argparse.ArgumentParser(
        description="Download Fortran assignment submissions from Google Sheets"
    )
    parser.add_argument("student_id", help="Student ID to search for")
    parser.add_argument("output_dir", help="Directory to save downloaded files")
    parser.add_argument(
        "--force", "-f", action="store_true", help="Overwrite existing files"
    )
    parser.add_argument(
        "--dry-run",
        "-n",
        action="store_true",
        help="Show what would be downloaded without actually downloading",
    )
    parser.add_argument(
        "--assignment",
        "-a",
        type=int,
        help="Download specific assignment number (1-5). If not specified, uses latest submission.",
    )
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    sheet_id = extract_sheet_id(SUBMISSION_SHEET_URL)

    try:
        all_rows = fetch_sheet_csv(sheet_id)
    except Exception as e:
        print(f"ERROR accessing Google Sheet: {e}", file=sys.stderr)
        sys.exit(1)

    matching_rows = []
    for row in all_rows:
        if len(row) >= 3 and row[2] == args.student_id:
            matching_rows.append(row)

    if not matching_rows:
        print(
            f"ERROR: No submission found for student ID: {args.student_id}",
            file=sys.stderr,
        )
        sys.exit(1)

    # Filter by assignment number if specified
    if args.assignment is not None:
        filtered_rows = []
        for row in matching_rows:
            if len(row) >= 4:
                try:
                    row_assignment = extract_assignment_number(row[3])
                    if row_assignment == args.assignment:
                        filtered_rows.append(row)
                except ValueError:
                    continue

        if not filtered_rows:
            print(
                f"ERROR: No assignment {args.assignment} submission found for student {args.student_id}",
                file=sys.stderr,
            )
            sys.exit(1)

        if len(filtered_rows) > 1:
            print(
                f"WARNING: Found {len(filtered_rows)} submissions for assignment {args.assignment}, "
                f"using latest (row {len(all_rows) - (len(all_rows) - filtered_rows.index(filtered_rows[-1]))})",
                file=sys.stderr,
            )

        latest_row = filtered_rows[-1]
    else:
        # Use latest submission (original behavior)
        if len(matching_rows) > 1:
            print(
                f"WARNING: Found {len(matching_rows)} submissions for student {args.student_id}, "
                f"using latest (row {len(all_rows) - (len(all_rows) - matching_rows.index(matching_rows[-1]))})",
                file=sys.stderr,
            )
        latest_row = matching_rows[-1]

    if len(latest_row) < 5:
        print("ERROR: Invalid row format in sheet", file=sys.stderr)
        sys.exit(1)

    kadai_text = latest_row[3]
    urls_text = latest_row[4]

    try:
        assignment_num = extract_assignment_number(kadai_text)
    except ValueError as e:
        print(f"ERROR: {e}", file=sys.stderr)
        sys.exit(1)

    urls = parse_drive_urls(urls_text)

    if not urls:
        print(f"ERROR: No URLs found for student {args.student_id}", file=sys.stderr)
        sys.exit(1)

    print(
        f"Found {len(urls)} file(s) for student {args.student_id}, 課題{assignment_num}"
    )

    if args.dry_run:
        print("(Dry run - no files will be downloaded)")

    success_count = 0
    skip_count = 0
    fail_count = 0

    for i, url in enumerate(urls, start=1):
        filename = f"{args.student_id}_assignment{assignment_num}_{i}.f90"
        output_path = output_dir / filename

        print(f"Downloading: {filename}...", end=" ")

        if args.dry_run:
            if output_path.exists():
                print("EXISTS")
                skip_count += 1
            else:
                print("WOULD DOWNLOAD")
                success_count += 1
        else:
            success, message = download_file(url, output_path, overwrite=args.force)
            print(message)

            if success:
                success_count += 1
            elif "EXISTS" in message:
                skip_count += 1
            else:
                fail_count += 1

    print(
        f"\nDone! {success_count} downloaded, {skip_count} skipped, {fail_count} failed"
    )


if __name__ == "__main__":
    main()
