use nom::branch::alt;
use nom::bytes::complete::{escaped_transform, take_while, take_while1};
use nom::character::complete::{char, multispace1, none_of};
use nom::combinator::{opt, value as nom_value};
use nom::error::{Error, ErrorKind};
use nom::multi::{many0, many0_count};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::{Err, IResult};

use crate::values::{DebugInfo, Value, ValueItem};

pub fn parse_value(input: &str, filename: Option<&str>) -> Result<Value, String> {
    match delimited(ws, |i| expr(filename, input, i), ws)(input) {
        Ok(("", value)) => Ok(value),
        Ok((rest, _)) => Err(format!("unexpected trailing input: {:?}", rest)),
        Err(e) => Err(format!("parse error: {}", e)),
    }
}

pub fn parse_all(input: &str, filename: Option<&str>) -> Result<Vec<Value>, String> {
    match preceded(ws, many0(terminated(|i| expr(filename, input, i), ws)))(input) {
        Ok(("", values)) => Ok(values),
        Ok((rest, _)) => Err(format!("unexpected trailing input: {:?}", rest)),
        Err(e) => Err(format!("parse error: {}", e)),
    }
}

// `remaining` is always a suffix of `origin`, so the difference of their
// start pointers is how many bytes we have already consumed.
fn byte_offset(origin: &str, remaining: &str) -> usize {
    remaining.as_ptr() as usize - origin.as_ptr() as usize
}

fn debug_at(filename: Option<&str>, origin: &str, offset: usize) -> DebugInfo {
    let consumed = &origin[..offset];
    let line_start = consumed.rfind('\n').map_or(0, |i| i + 1);
    DebugInfo {
        filename: filename.map(str::to_string),
        line_no: consumed.matches('\n').count() + 1,
        char_offset: offset - line_start + 1,
    }
}

// Skips whitespace and `;` line comments.
fn ws(input: &str) -> IResult<&str, ()> {
    let comment = pair(char(';'), take_while(|c: char| c != '\n' && c != '\r'));
    nom_value(
        (),
        many0_count(alt((nom_value((), multispace1), nom_value((), comment)))),
    )(input)
}

fn expr<'a>(filename: Option<&'a str>, origin: &'a str, input: &'a str) -> IResult<&'a str, Value> {
    let start = byte_offset(origin, input);
    let (rest, value) = alt((
        string_literal,
        |i| list(filename, origin, i),
        |i| quoted(filename, origin, i),
        atom,
    ))(input)?;
    Ok((rest, value.with_debug(debug_at(filename, origin, start))))
}

fn is_atom_char(c: char) -> bool {
    !c.is_whitespace() && !"()\";'".contains(c)
}

fn quoted<'a>(
    filename: Option<&'a str>,
    origin: &'a str,
    input: &'a str,
) -> IResult<&'a str, Value> {
    let (input, _) = char('\'')(input)?;
    let (input, inner) = expr(filename, origin, input)?;
    Ok((input, Value::list([Value::symbol("quote"), inner])))
}

fn atom(input: &str) -> IResult<&str, Value> {
    let (rest, token) = take_while1(is_atom_char)(input)?;
    if token == "." {
        return Err(Err::Error(Error::new(input, ErrorKind::Verify)));
    }
    if token == "#t" {
        return Ok((rest, Value::boolean(true)));
    }
    if token == "#f" {
        return Ok((rest, Value::boolean(false)));
    }
    let looks_numeric = token.chars().next().map_or(false, |c| {
        c.is_ascii_digit() || c == '+' || c == '-' || c == '.'
    });
    if looks_numeric {
        if let Ok(n) = token.parse::<f64>() {
            return Ok((rest, Value::number(n)));
        }
    }
    Ok((rest, Value::symbol(token.to_string())))
}

fn string_literal(input: &str) -> IResult<&str, Value> {
    let (input, _) = char('"')(input)?;
    let (input, contents) = opt(escaped_transform(
        none_of("\"\\"),
        '\\',
        alt((
            nom_value("\\", char('\\')),
            nom_value("\"", char('"')),
            nom_value("\n", char('n')),
            nom_value("\t", char('t')),
        )),
    ))(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, Value::string(contents.unwrap_or_default())))
}

fn list<'a>(filename: Option<&'a str>, origin: &'a str, input: &'a str) -> IResult<&'a str, Value> {
    let (input, _) = char('(')(input)?;
    let (input, _) = ws(input)?;
    let (input, items) = many0(terminated(|i| expr(filename, origin, i), ws))(input)?;
    let (input, tail) = opt(preceded(
        terminated(char('.'), multispace1),
        terminated(|i| expr(filename, origin, i), ws),
    ))(input)?;
    let (input, _) = char(')')(input)?;
    let items = desugar_bang(items);
    let tail = tail.unwrap_or_else(Value::null);
    let value = items
        .into_iter()
        .rev()
        .fold(tail, |cdr, car| Value::pair(car, cdr));
    Ok((input, value))
}

// `(!x rest...)` and `(! x rest...)` desugar to `(macroexpand x rest...)`.
fn desugar_bang(items: Vec<Value>) -> Vec<Value> {
    let (suffix, debug) = match items.first() {
        Some(head) => match head.get() {
            ValueItem::Symbol(name) => match name.strip_prefix('!') {
                Some(rest) => (rest.to_string(), head.debug.clone()),
                None => return items,
            },
            _ => return items,
        },
        None => return items,
    };
    let tag = |value: Value| Value {
        debug: debug.clone(),
        ..value
    };
    let mut out = vec![tag(Value::symbol("macroexpand"))];
    if !suffix.is_empty() {
        out.push(tag(Value::symbol(suffix)));
    }
    out.extend(items.into_iter().skip(1));
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Result<Value, String> {
        parse_value(input, None)
    }

    #[test]
    fn parses_symbol() {
        assert_eq!(parse("foo"), Ok(Value::symbol("foo")));
        assert_eq!(parse("+"), Ok(Value::symbol("+")));
        assert_eq!(parse("nil?"), Ok(Value::symbol("nil?")));
    }

    #[test]
    fn parses_number() {
        assert_eq!(parse("42"), Ok(Value::number(42.0)));
        assert_eq!(parse("-3.5"), Ok(Value::number(-3.5)));
        assert_eq!(parse("1e3"), Ok(Value::number(1000.0)));
    }

    #[test]
    fn parses_booleans() {
        assert_eq!(parse("#t"), Ok(Value::boolean(true)));
        assert_eq!(parse("#f"), Ok(Value::boolean(false)));
    }

    #[test]
    fn desugars_quote_shorthand() {
        assert_eq!(parse("'foo"), parse("(quote foo)"));
        assert_eq!(parse("'(a b)"), parse("(quote (a b))"));
        assert_eq!(parse("'()"), parse("(quote ())"));
        assert_eq!(parse("'(a 'b)"), parse("(quote (a (quote b)))"));
    }

    #[test]
    fn desugars_bang_to_macroexpand() {
        assert_eq!(parse("(!foo bar baz)"), parse("(macroexpand foo bar baz)"));
        assert_eq!(parse("(!(f x) bar)"), parse("(macroexpand (f x) bar)"));
    }

    #[test]
    fn ignores_comments() {
        assert_eq!(
            parse("; leading\n(a ; inline\n  b) ; trailing"),
            Ok(Value::list([Value::symbol("a"), Value::symbol("b")]))
        );
    }

    #[test]
    fn parses_string() {
        assert_eq!(parse("\"hello\""), Ok(Value::string("hello")));
        assert_eq!(parse("\"\""), Ok(Value::string("")));
        assert_eq!(parse("\"a\\\"b\\nc\""), Ok(Value::string("a\"b\nc")));
    }

    #[test]
    fn parses_empty_list_as_null() {
        assert_eq!(parse("()"), Ok(Value::null()));
    }

    #[test]
    fn parses_proper_list() {
        assert_eq!(
            parse("(1 2 3)"),
            Ok(Value::list([
                Value::number(1.0),
                Value::number(2.0),
                Value::number(3.0),
            ]))
        );
    }

    #[test]
    fn parses_nested_list() {
        assert_eq!(
            parse("(add (mul 2 3) x)"),
            Ok(Value::list([
                Value::symbol("add"),
                Value::list([Value::symbol("mul"), Value::number(2.0), Value::number(3.0),]),
                Value::symbol("x"),
            ]))
        );
    }

    #[test]
    fn parses_dotted_pair() {
        assert_eq!(
            parse("(1 . 2)"),
            Ok(Value::pair(Value::number(1.0), Value::number(2.0)))
        );
    }

    #[test]
    fn ignores_surrounding_whitespace() {
        assert_eq!(parse("  \n foo \t "), Ok(Value::symbol("foo")));
    }

    #[test]
    fn rejects_trailing_input() {
        assert!(parse("foo bar").is_err());
    }

    #[test]
    fn rejects_unclosed_list() {
        assert!(parse("(1 2").is_err());
    }
}
