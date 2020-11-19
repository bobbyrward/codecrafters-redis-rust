use bytes::{Buf, BytesMut};

use crate::error::EngineError;

/// Helper to simplify stringifying bytes
fn utf8(bytes: &[u8]) -> Result<String, EngineError> {
    Ok(std::str::from_utf8(bytes)
        .map_err(|_| EngineError::unknown(&format!("utf8 parsing error: {:?}", bytes)))?
        .to_string())
}

/// Try to read up to the first carriage return line feed
fn take_crlf<'a>(
    buffer: &'a mut BytesMut,
    position: usize,
) -> Result<Option<(Vec<u8>, usize)>, EngineError> {
    if buffer.len() < position + 1 {
        return Ok(None);
    }

    let newline_offset = buffer[..]
        .iter()
        .skip(position)
        .zip(buffer[position + 1..].iter())
        .position(|(cr, lf)| *cr == b'\r' && *lf == b'\n');

    if let Some(offset) = newline_offset {
        Ok(Some((
            buffer[position..position + offset].to_owned(),
            position + offset + 2,
        )))
    } else {
        Ok(None)
    }
}

/// Try to read a RESP simple string from buffer
///
/// Position is expected to be __after__ the '*'
fn resp_read_simple_string(
    buffer: &mut BytesMut,
    position: usize,
) -> Result<Option<(RespValue, usize)>, EngineError> {
    let (value, new_position) = {
        if let Some(found) = take_crlf(buffer, position)? {
            found
        } else {
            return Ok(None);
        }
    };

    Ok(Some((RespValue::SimpleString(utf8(&value)?), new_position)))
}

/// Try to read a RESP integer from buffer
///
/// Position is expected to be __after__ the '*'
fn resp_read_integer(
    buffer: &mut BytesMut,
    position: usize,
) -> Result<Option<(RespValue, usize)>, EngineError> {
    let (value, new_position) = {
        if let Some(found) = take_crlf(buffer, position)? {
            found
        } else {
            return Ok(None);
        }
    };

    Ok(Some((
        RespValue::Integer(digits_to_int(&value)?),
        new_position,
    )))
}

/// Try to read a RESP error from buffer
///
/// Position is expected to be __after__ the '*'
fn resp_read_error(
    buffer: &mut BytesMut,
    position: usize,
) -> Result<Option<(RespValue, usize)>, EngineError> {
    let (value, new_position) = {
        if let Some(found) = take_crlf(buffer, position)? {
            found
        } else {
            return Ok(None);
        }
    };

    Ok(Some((RespValue::Error(utf8(&value)?), new_position)))
}

/// Try to read a RESP bulkstring from buffer
///
/// Position is expected to be __after__ the '$'
fn resp_read_bulkstring(
    buffer: &mut BytesMut,
    position: usize,
) -> Result<Option<(RespValue, usize)>, EngineError> {
    let (length, new_position) = {
        if let Some(found) = take_crlf(buffer, position)? {
            found
        } else {
            return Ok(None);
        }
    };

    let length = {
        let n = digits_to_int(&length)?;
        if n < 0 {
            return Ok(Some((RespValue::Null, new_position)));
        }
        n as usize
    };

    if buffer.len() < (new_position + length + 2) {
        return Ok(None);
    }

    Ok(Some((
        RespValue::BulkString(buffer[new_position..new_position + length].to_owned()),
        new_position + length + 2,
    )))
}

/// Try to read a RESP array from buffer
///
/// Position is expected to be __after__ the '*'
fn resp_read_array(
    buffer: &mut BytesMut,
    position: usize,
) -> Result<Option<(RespValue, usize)>, EngineError> {
    let (length, new_position) = {
        if let Some(found) = take_crlf(buffer, position)? {
            found
        } else {
            return Ok(None);
        }
    };

    let length = {
        let n = digits_to_int(&length)?;
        if n < 0 {
            return Ok(Some((RespValue::Null, new_position)));
        }
        n as usize
    };

    let mut values = Vec::with_capacity(length);
    let mut new_position = new_position;

    for _ in 0..length {
        let (value, next_position) = {
            if let Some(found) = resp_read_value(buffer, new_position)? {
                found
            } else {
                return Ok(None);
            }
        };

        values.push(value);
        new_position = next_position;
    }

    Ok(Some((RespValue::Array(values), new_position)))
}

fn resp_read_value(
    buffer: &mut BytesMut,
    position: usize,
) -> Result<Option<(RespValue, usize)>, EngineError> {
    // The smallest possible value should be 4 long.
    // 1 for type
    // 1 for string or int
    // 2 for terminator
    if buffer.len() < 4 {
        return Ok(None);
    }

    match buffer[position] {
        // Array
        b'*' => resp_read_array(buffer, position + 1),
        // BulkString
        b'$' => resp_read_bulkstring(buffer, position + 1),
        // Integer
        b':' => resp_read_integer(buffer, position + 1),
        // SimpleString
        b'+' => resp_read_simple_string(buffer, position + 1),
        // Error
        b'-' => resp_read_error(buffer, position + 1),
        // w/e else
        x => Err(EngineError::unknown(&format!("Invalid prefix: '{}'", x))),
    }
}

/// Encode value as a string
fn resp_encode(value: &RespValue) -> Result<Vec<u8>, EngineError> {
    Ok(match value {
        RespValue::SimpleString(s) | RespValue::Error(s) => {
            if s.contains('\n') {
                return Err(EngineError::unknown(&format!(
                    "Value is not binary safe: {}",
                    s
                )));
            }

            format!("+{}\r\n", s).as_bytes().iter().copied().collect()
        }
        RespValue::Integer(i) => format!(":{}\r\n", i).as_bytes().iter().copied().collect(),
        RespValue::BulkString(bytes) => {
            let mut encoded = Vec::new();
            encoded.extend_from_slice(format!("${}\r\n", bytes.len()).as_bytes());
            encoded.extend_from_slice(bytes);
            encoded.extend_from_slice(b"\r\n");
            encoded
        }
        RespValue::Array(values) => {
            let mut encoded = Vec::new();
            encoded.extend_from_slice(format!("*{}\r\n", values.len()).as_bytes());

            for item in values.iter() {
                encoded.extend_from_slice(&resp_encode(item)?);
            }

            encoded
        }
        RespValue::Null => String::from("*-1\r\n").as_bytes().iter().copied().collect(),
    })
}

fn from_buf(buffer: &mut BytesMut) -> Result<Option<RespValue>, EngineError> {
    if let Some((value, position)) = resp_read_value(buffer, 0)? {
        buffer.advance(position);
        Ok(Some(value))
    } else {
        Ok(None)
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub(crate) enum RespValue {
    SimpleString(String),
    Error(String),
    Integer(i64),
    BulkString(Vec<u8>),
    Array(Vec<Self>),
    Null,
}

impl RespValue {
    pub(crate) fn as_bulk_string(&self) -> Option<&Vec<u8>> {
        if let Self::BulkString(bytes) = self {
            Some(bytes)
        } else {
            None
        }
    }
    pub(crate) fn as_str(&self) -> Result<&str, EngineError> {
        match self {
            Self::SimpleString(s) => Ok(&s),
            Self::Error(s) => Ok(&s),
            Self::BulkString(bytes) => std::str::from_utf8(bytes).map_err(|e| {
                EngineError::unknown(&format!("Bytes are not utf8: {:?} {:?}", bytes, e))
            }),
            _ => Err(EngineError::unknown("RespValue isn't stringy")),
        }
    }

    pub(crate) fn len(&self) -> Result<usize, EngineError> {
        match self {
            Self::SimpleString(s) => Ok(s.len()),
            Self::Error(s) => Ok(s.len()),
            Self::BulkString(bytes) => Ok(bytes.len()),
            Self::Array(arr) => Ok(arr.len()),
            _ => Err(EngineError::unknown("RespValue isn't lengthy")),
        }
    }

    pub(crate) fn is_array(&self) -> bool {
        matches!(self, Self::Array(_))
    }

    pub(crate) fn simple_string(value: &str) -> Self {
        Self::SimpleString(value.to_string())
    }

    pub(crate) fn error(value: &str) -> Self {
        Self::Error(value.to_string())
    }

    pub(crate) fn integer(value: i64) -> Self {
        Self::Integer(value)
    }

    pub(crate) fn from_buf(buffer: &mut BytesMut) -> Result<Option<Self>, EngineError> {
        from_buf(buffer)
    }

    pub(crate) fn encode(&self) -> Result<Vec<u8>, EngineError> {
        resp_encode(self)
    }
}

/// Convert a slice of numeric bytes to an integer
fn digits_to_int(mut digits: &[u8]) -> Result<i64, EngineError> {
    let mut sign = 1;

    if digits.len() > 0 && digits[0] == b'-' {
        sign = -1;
        digits = &digits[1..];
    }

    if digits.iter().any(|byte| *byte < b'0' || *byte > b'9') {
        eprintln!("{:?} is non numeric", digits);
        return Err(EngineError::Unknown {
            message: String::from("Non numeric digit in string"),
        });
    }

    Ok(digits
        .iter()
        .rev()
        .enumerate()
        .map(|(exp, byte)| {
            let digit: i64 = (byte - b'0').into();
            let tens = 10i64.pow(exp as u32);

            digit * tens
        })
        .sum::<i64>()
        * sign)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::error::Error;

    type TestResult = Result<(), Box<dyn Error + Send + Sync + 'static>>;

    #[test]
    fn test_digits_to_int() {
        assert!(digits_to_int(&[b'c']).is_err());

        let tests = [
            (vec![b'0'], 0),
            (vec![b'9'], 9),
            (vec![b'1', b'0'], 10),
            (vec![b'1', b'9'], 19),
            (vec![b'1', b'0', b'0'], 100),
            (vec![b'1', b'9', b'9'], 199),
            (vec![b'1', b'0', b'0', b'0', b'0', b'0', b'0'], 1000000),
        ];

        for (digits, expected) in &tests {
            assert_eq!(digits_to_int(&digits).unwrap(), *expected);
        }
    }

    #[test]
    fn test_take_crlf() -> TestResult {
        let tests = [
            ("Hello", None),
            ("\n", None),
            (
                "Hello\r\nWorld!\r\n",
                Some((vec![b'H', b'e', b'l', b'l', b'o'], 7)),
            ),
        ];

        for (source, expected) in &tests {
            let mut buffer = BytesMut::from(&source[..]);

            assert_eq!(take_crlf(&mut buffer, 0)?, *expected);
        }

        let mut buffer = BytesMut::from("Hello\r\nWorld!\r\n");
        let (first_value, first_offset) = take_crlf(&mut buffer, 0)?.unwrap();
        let (second_value, second_offset) = take_crlf(&mut buffer, first_offset)?.unwrap();

        assert_eq!(first_value, b"Hello");
        assert_eq!(first_offset, 7);
        assert_eq!(second_value, b"World!");
        assert_eq!(second_offset, 15);
        assert_eq!(&buffer[second_offset - 1..second_offset], b"\n");

        Ok(())
    }

    #[test]
    fn test_from_bytes() -> TestResult {
        let tests = [
            (
                "+Hello, World!\r\n",
                RespValue::SimpleString(String::from("Hello, World!")),
            ),
            (
                "-Error Message\r\n",
                RespValue::Error(String::from("Error Message")),
            ),
            ("$-1\r\n", RespValue::Null),
            ("$1\r\n?\r\n", RespValue::BulkString(vec![b'?'])),
            (
                "$4\r\nBulk\r\n",
                RespValue::BulkString(vec![b'B', b'u', b'l', b'k']),
            ),
            (":0\r\n", RespValue::Integer(0)),
            (":12\r\n", RespValue::Integer(12)),
            (":-1234\r\n", RespValue::Integer(-1234)),
            (":9999999\r\n", RespValue::Integer(9999999)),
            ("*-1\r\n", RespValue::Null),
            ("*0\r\n", RespValue::Array(vec![])),
            (
                "*1\r\n+Item 1\r\n",
                RespValue::Array(vec![RespValue::SimpleString(String::from("Item 1"))]),
            ),
            (
                "*2\r\n+Item 1\r\n:456\r\n",
                RespValue::Array(vec![
                    RespValue::SimpleString(String::from("Item 1")),
                    RespValue::Integer(456),
                ]),
            ),
            (
                "*2\r\n+Item 1\r\n:456\r\n",
                RespValue::Array(vec![
                    RespValue::SimpleString(String::from("Item 1")),
                    RespValue::Integer(456),
                ]),
            ),
            (
                "*3\r\n$6\r\nst\ning\r\n*2\r\n+SubItem1\r\n+SubItem2\r\n-Error\r\n",
                RespValue::Array(vec![
                    RespValue::BulkString(b"st\ning".to_vec()),
                    RespValue::Array(vec![
                        RespValue::SimpleString(String::from("SubItem1")),
                        RespValue::SimpleString(String::from("SubItem2")),
                    ]),
                    RespValue::Error(String::from("Error")),
                ]),
            ),
        ];

        for (source, expected) in &tests {
            let mut buffer = BytesMut::from(source.as_bytes());
            let value = from_buf(&mut buffer)?.expect("should have been found");

            assert_eq!(value, *expected);
        }

        Ok(())
    }
}
