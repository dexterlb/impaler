use std::env;
use std::fs;
use std::path::Path;

// Embeds every file under ../ild-code and generates one `#[test]` per case in
// each `expr-tests/*.ild` file.
fn main() {
    let manifest = env::var("CARGO_MANIFEST_DIR").unwrap();
    let root = Path::new(&manifest)
        .join("..")
        .join("ild-code")
        .canonicalize()
        .expect("ild-code directory");

    println!("cargo:rerun-if-changed={}", root.display());

    let mut files: Vec<(String, String)> = Vec::new();
    collect(&root, &root, &mut files);
    files.sort();

    let mut out = String::new();
    out.push_str("pub static ILD_FILES: &[(&str, &str)] = &[\n");
    for (path, content) in &files {
        out.push_str(&format!("    ({:?}, {:?}),\n", path, content));
    }
    out.push_str("];\n\n");

    for (path, content) in &files {
        if let Some(stem) = path
            .strip_prefix("expr-tests/")
            .and_then(|p| p.strip_suffix(".ild"))
        {
            for case in case_names(content) {
                out.push_str(&format!(
                    "#[test]\nfn {}_{}() {{\n    run_case({:?}, {:?});\n}}\n\n",
                    ident(stem),
                    ident(&case),
                    path,
                    case,
                ));
            }
        }
    }

    let dest = Path::new(&env::var("OUT_DIR").unwrap()).join("ild_generated.rs");
    fs::write(dest, out).unwrap();
}

fn collect(root: &Path, dir: &Path, files: &mut Vec<(String, String)>) {
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            collect(root, &path, files);
        } else if path.is_file() {
            let rel = path
                .strip_prefix(root)
                .unwrap()
                .to_string_lossy()
                .replace('\\', "/");
            let content = fs::read_to_string(&path).unwrap_or_default();
            files.push((rel, content));
        }
    }
}

// Extracts case names by scanning for `(case "..."`, ignoring `;` comments.
fn case_names(content: &str) -> Vec<String> {
    let stripped: String = content
        .lines()
        .map(|line| match line.find(';') {
            Some(i) => &line[..i],
            None => line,
        })
        .collect::<Vec<_>>()
        .join("\n");

    let mut names = Vec::new();
    let mut rest = stripped.as_str();
    while let Some(pos) = rest.find("(case") {
        rest = &rest[pos + "(case".len()..];
        let after = rest.trim_start();
        if let Some(quoted) = after.strip_prefix('"') {
            if let Some(end) = quoted.find('"') {
                names.push(quoted[..end].to_string());
                rest = &quoted[end + 1..];
            }
        }
    }
    names
}

fn ident(name: &str) -> String {
    let mut out: String = name
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    if out.is_empty() || out.starts_with(|c: char| c.is_ascii_digit()) {
        out.insert(0, '_');
    }
    out
}
