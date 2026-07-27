use nom::branch::alt;
use nom::bytes::complete::{escaped_transform, take_while, take_while1};
use nom::character::complete::{char, multispace1, none_of};
use nom::combinator::{opt, value as nom_value};
use nom::error::{Error, ErrorKind};
use nom::multi::{many0, many0_count};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::{Err, IResult};

use crate::values::Value;

pub fn parse_value(input: &str) -> Result<Value, String> {
    match delimited(ws, expr, ws)(input) {
        Ok(("", value)) => Ok(value),
        Ok((rest, _)) => Err(format!("unexpected trailing input: {:?}", rest)),
        Err(e) => Err(format!("parse error: {}", e)),
    }
}

// Parses a sequence of top-level forms.
pub fn parse_all(input: &str) -> Result<Vec<Value>, String> {
    match preceded(ws, many0(terminated(expr, ws)))(input) {
        Ok(("", values)) => Ok(values),
        Ok((rest, _)) => Err(format!("unexpected trailing input: {:?}", rest)),
        Err(e) => Err(format!("parse error: {}", e)),
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

fn expr(input: &str) -> IResult<&str, Value> {
    alt((string_literal, list, atom))(input)
}

fn is_atom_char(c: char) -> bool {
    !c.is_whitespace() && !"()\";".contains(c)
}

fn atom(input: &str) -> IResult<&str, Value> {
    let (rest, token) = take_while1(is_atom_char)(input)?;
    if token == "." {
        return Err(Err::Error(Error::new(input, ErrorKind::Verify)));
    }
    if token == "#t" {
        return Ok((rest, Value::Bool(true)));
    }
    if token == "#f" {
        return Ok((rest, Value::Bool(false)));
    }
    let looks_numeric = token.chars().next().map_or(false, |c| {
        c.is_ascii_digit() || c == '+' || c == '-' || c == '.'
    });
    if looks_numeric {
        if let Ok(n) = token.parse::<f64>() {
            return Ok((rest, Value::Number(n)));
        }
    }
    Ok((rest, Value::Symbol(token.to_string())))
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
    Ok((input, Value::String(contents.unwrap_or_default())))
}

fn list(input: &str) -> IResult<&str, Value> {
    let (input, _) = char('(')(input)?;
    let (input, _) = ws(input)?;
    let (input, items) = many0(terminated(expr, ws))(input)?;
    let (input, tail) = opt(preceded(
        terminated(char('.'), multispace1),
        terminated(expr, ws),
    ))(input)?;
    let (input, _) = char(')')(input)?;
    let tail = tail.unwrap_or(Value::Null);
    let value = items
        .into_iter()
        .rev()
        .fold(tail, |cdr, car| Value::pair(car, cdr));
    Ok((input, value))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Result<Value, String> {
        parse_value(input)
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
        assert_eq!(parse("()"), Ok(Value::Null));
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
