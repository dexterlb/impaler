use std::fs;
use std::process::exit;

const STEP: usize = 2;

// warning: this file is generated slop

fn main() {
    let paths: Vec<String> = std::env::args().skip(1).collect();
    if paths.is_empty() {
        eprintln!("usage: fmt_ild <file.ild> ...");
        exit(2);
    }
    for path in &paths {
        let source = fs::read_to_string(path).unwrap_or_else(|e| {
            eprintln!("{}: read error: {}", path, e);
            exit(1);
        });
        fs::write(path, reindent(&source)).unwrap_or_else(|e| {
            eprintln!("{}: write error: {}", path, e);
            exit(1);
        });
        println!("indented {}", path);
    }
}

fn reindent(source: &str) -> String {
    let mut stack: Vec<usize> = Vec::new();
    let mut out = String::new();
    for line in source.lines() {
        let content = line.trim();
        if content.is_empty() {
            out.push('\n');
            continue;
        }
        let indent = line_indent(&stack, content);
        out.push_str(&" ".repeat(indent));
        out.push_str(content);
        out.push('\n');
        scan(&mut stack, content, indent);
    }
    out
}

fn line_indent(stack: &[usize], content: &str) -> usize {
    let leading_closes = content.chars().take_while(|&c| c == ')').count();
    if leading_closes > 0 {
        let idx = stack.len().saturating_sub(leading_closes);
        stack.get(idx).copied().unwrap_or(0)
    } else {
        stack.last().map_or(0, |top| top + STEP)
    }
}

fn scan(stack: &mut Vec<usize>, content: &str, indent: usize) {
    let mut in_string = false;
    let mut escaped = false;
    for ch in content.chars() {
        if in_string {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            ';' => break, // rest of the line is a comment
            '"' => in_string = true,
            '(' => stack.push(indent),
            ')' => {
                stack.pop();
            }
            _ => {}
        }
    }
}
