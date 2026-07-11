use std::env;
use std::fs;
use std::io::{self, BufRead};
use std::path::Path;
use std::process::exit;

/// A line of analyzed lisp code
#[derive(Debug, Clone)]
struct LineInfo {
    num: usize,
    indent: usize,
    paren_balance: i32,
    is_blank: bool,
    in_string: bool,
    content: String,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let start_dir = args.get(1).map(|s| s.as_str()).unwrap_or(".");

    let files = collect_lisp_files(start_dir);
    println!("=== Paren Check: {} file(s) ===", files.len());

    let mut error_count = 0;
    let mut processed = 0;

    for filepath in &files {
        match check_file(filepath) {
            Ok(issues) => {
                if issues.is_empty() {
                    processed += 1;
                } else {
                    println!("\n{}", filepath);
                    for issue in issues {
                        println!("  {}", issue);
                    }
                    error_count += 1;
                    processed += 1;
                }
            }
            Err(e) => {
                eprintln!("Error reading {}: {}", filepath, e);
                error_count += 1;
                processed += 1;
            }
        }
    }

    println!("\n=== {} checked, {} with issues ===", processed, error_count);
    exit(if error_count > 0 { 1 } else { 0 });
}

fn collect_lisp_files(dir: &str) -> Vec<String> {
    let mut files = Vec::new();
    collect_recursive(Path::new(dir), &mut files);
    files.sort();
    files
}

fn collect_recursive(path: &Path, files: &mut Vec<String>) {
    if path.is_dir() {
        if let Ok(entries) = fs::read_dir(path) {
            for entry in entries.flatten() {
                collect_recursive(&entry.path(), files);
            }
        }
    } else if path.extension().map(|e| e == "lisp").unwrap_or(false) {
        files.push(path.to_string_lossy().to_string());
    }
}

fn check_file(filepath: &str) -> io::Result<Vec<String>> {
    let lines = analyze_file(filepath)?;

    if lines.is_empty() {
        return Ok(vec![]);
    }

    let final_balance = lines.last().unwrap().paren_balance;
    let mut issues = Vec::new();

    // Report file-level unbalanced
    if final_balance != 0 {
        let msg = if final_balance > 0 {
            format!("  [unbalanced] {} unclosed opening paren(s)", final_balance)
        } else {
            format!("  [unbalanced] {} extra closing paren(s)", -final_balance)
        };
        issues.push(msg);
    }

    // Track balance and string state BEFORE each line
    let mut balance_at_line_start = 0i32;
    let mut in_string_at_line_start = false;

    for (idx, line) in lines.iter().enumerate() {
        if !line.is_blank {
            // Only flag issues if we're NOT inside a multi-line string
            if !in_string_at_line_start {
                // 1. Line at column 0 that LOOKS like a new top-level form
                // (starts with '(' and has balance > 0) - previous form didn't close
                let starts_with_paren = line.content.trim_start().starts_with('(');
                if line.indent == 0 && starts_with_paren && balance_at_line_start > 0 {
                    // Find the previous top-level form start
                    let mut prev_form_start = 0;
                    for prev in lines[..idx].iter().rev() {
                        if prev.indent == 0 && !prev.is_blank {
                            prev_form_start = prev.num;
                            break;
                        }
                    }
                    let ctx = get_context(&lines, prev_form_start);
                    issues.push(format!(
                        "Line {} [missing-close]: Previous form (line {}) unclosed (balance {}), missing ')'\n  Context:\n{}",
                        line.num, prev_form_start, balance_at_line_start, ctx
                    ));
                }

                // 2. Line at column 0 with balance < 0 means extra close before this line
                if line.indent == 0 && balance_at_line_start < 0 {
                    let ctx = get_context(&lines, line.num);
                    issues.push(format!(
                        "Line {} [extra-close]: Paren balance negative at start of line (extra ')' somewhere before)\n  Context:\n{}",
                        line.num, ctx
                    ));
                }
            }
        }

        // Update state for next line
        balance_at_line_start = line.paren_balance;
        in_string_at_line_start = line.in_string;
    }

    Ok(issues)
}

fn analyze_file(filepath: &str) -> io::Result<Vec<LineInfo>> {
    let file = fs::File::open(filepath)?;
    let reader = io::BufReader::new(file);
    let mut lines = Vec::new();
    let mut balance = 0i32;
    let mut in_string = false;
    let mut escaped = false;

    for (idx, line_result) in reader.lines().enumerate() {
        let content = line_result?;
        let (new_balance, new_in_string, new_escaped) =
            count_parens(&content, balance, in_string, escaped);

        lines.push(LineInfo {
            num: idx + 1,
            indent: leading_indent(&content),
            paren_balance: new_balance,
            is_blank: is_blank(&content),
            in_string: new_in_string,
            content,
        });

        balance = new_balance;
        in_string = new_in_string;
        escaped = new_escaped;
    }

    Ok(lines)
}

fn count_parens(line: &str, start_count: i32, start_in_string: bool, start_escaped: bool) -> (i32, bool, bool) {
    let mut count = start_count;
    let mut in_string = start_in_string;
    let mut escaped = start_escaped;

    let mut characters = line.chars().peekable();

    while let Some(ch) = characters.next() {
        if escaped {
            escaped = false;
            continue;
        }

        if in_string {
            if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }

        // A Common Lisp character literal such as #\( contains syntax that
        // must not affect the surrounding form's parenthesis balance.
        if ch == '#' && characters.peek() == Some(&'\\') {
            characters.next();
            if let Some(character) = characters.next() {
                if character.is_alphanumeric() {
                    while characters
                        .peek()
                        .is_some_and(|next| next.is_alphanumeric() || *next == '-')
                    {
                        characters.next();
                    }
                }
            }
            continue;
        }

        match ch {
            '"' => in_string = true,
            ';' => break, // Comment starts, ignore rest of line
            '(' => count += 1,
            ')' => count -= 1,
            _ => {}
        }
    }

    (count, in_string, escaped)
}

fn is_blank(line: &str) -> bool {
    line.chars().all(|c| c.is_whitespace())
}

fn leading_indent(line: &str) -> usize {
    line.chars().take_while(|c| c.is_whitespace()).count()
}

fn get_context(lines: &[LineInfo], target_line: usize) -> String {
    let target_idx = target_line.saturating_sub(1);

    // Find start (first blank line before, or beginning)
    let start = (0..target_idx)
        .rev()
        .find(|&i| lines[i].is_blank)
        .map(|i| i + 1)
        .unwrap_or(0);

    // Find end (first blank line after, or end)
    let end = (target_idx..lines.len())
        .find(|&i| lines[i].is_blank)
        .unwrap_or(lines.len())
        .saturating_sub(1);

    let mut result = String::new();
    for i in start..=end.min(lines.len() - 1) {
        if i < lines.len() {
            result.push_str(&format!("  {}: {}\n", lines[i].num, lines[i].content));
        }
    }
    result
}
