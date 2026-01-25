// VHDL characters.
// Copyright (C) 2026 Tristan Gingold
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <gnu.org/licenses>.

#![allow(clippy::many_single_char_names)]
#![allow(unused)]

use crate::types::VhdlStd;

/// VHDL character classifications and helpers (Latin-1 aware).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum CharKind {
    Invalid = 0,
    FormatEffector = 1,
    LowerCaseLetter = 2,
    UpperCaseLetter = 3,
    Digit = 4,
    SpecialCharacter = 5,
    SpaceCharacter = 6,
    OtherSpecialCharacter = 7,
}

const TAB : u8 = 0x09;
const LF  : u8 = 0x0A;  // \n
const VT  : u8 = 0x0B;
const FF  : u8 = 0x0C;
const CR  : u8 = 0x0D;  // \r
const NBSP : u8 = 0xA0;

const LATIN_CAPITAL_A_WITH_GRAVE: u8 = 0xC0; // À
const LATIN_CAPITAL_A_WITH_CUTE: u8 = 0xC1; // Á
const LATIN_CAPITAL_A_WITH_CIRCUMFLEX: u8 = 0xC2; // Â
const LATIN_CAPITAL_A_WITH_TILDE: u8 = 0xC3; // Ã
const LATIN_CAPITAL_A_WITH_DIAERESIS: u8 = 0xC4; // Ä
const LATIN_CAPITAL_A_WITH_RING_ABOVE: u8 = 0xC5; // Å
const LATIN_CAPITAL_AE: u8 = 0xC6; // Æ
const LATIN_CAPITAL_C_WITH_CEDILLA: u8 = 0xC7; // Ç
const LATIN_CAPITAL_E_WITH_GRAVE: u8 = 0xC8; // È
const LATIN_CAPITAL_E_WITH_CUTE: u8 = 0xC9; // É
const LATIN_CAPITAL_E_WITH_CIRCUMFLEX: u8 = 0xCA; // Ê
const LATIN_CAPITAL_E_WITH_DIAERESIS: u8 = 0xCB; // Ë
const LATIN_CAPITAL_I_WITH_GRAVE: u8 = 0xCC; // Ì
const LATIN_CAPITAL_I_WITH_CUTE: u8 = 0xCD; // Í
const LATIN_CAPITAL_I_WITH_CIRCUMFLEX: u8 = 0xCE; // Î
const LATIN_CAPITAL_I_WITH_DIAERESIS: u8 = 0xCF; // Ï
const LATIN_CAPITAL_ETH: u8 = 0xD0; // Ð
const LATIN_CAPITAL_N_WITH_TILDE: u8 = 0xD1; // Ñ
const LATIN_CAPITAL_O_WITH_GRAVE: u8 = 0xD2; // Ò
const LATIN_CAPITAL_O_WITH_CUTE: u8 = 0xD3; // Ó
const LATIN_CAPITAL_O_WITH_CIRCUMFLEX: u8 = 0xD4; // Ô
const LATIN_CAPITAL_O_WITH_TILDE: u8 = 0xD5; // Õ
const LATIN_CAPITAL_O_DIAERESIS: u8 = 0xD6; // Ö
//  0xD7 is the multiplication sign (×)
const LATIN_CAPITAL_O_STROKE: u8 = 0xD8; // Ø
const LATIN_CAPITAL_U_WITH_GRAVE: u8 = 0xD9; // Ù
const LATIN_CAPITAL_U_WITH_CUTE: u8 = 0xDA; // Ú
const LATIN_CAPITAL_U_WITH_CIRCUMFLEX: u8 = 0xDB; // Û
const LATIN_CAPITAL_U_WITH_DIAERESIS: u8 = 0xDC; // Ü
const LATIN_CAPITAL_Y_WITH_CUTE: u8 = 0xDD; // Ý
const LATIN_CAPITAL_THORN: u8 = 0xDE; // Þ

const LATIN_SMALL_SHARP_S: u8 = 0xDF; // ß
const LATIN_SMALL_A_WITH_GRAVE: u8 = 0xE0; // à
const LATIN_SMALL_A_WITH_CUTE: u8 = 0xE1; // á
const LATIN_SMALL_A_WITH_CIRCUMFLEX: u8 = 0xE2; // â
const LATIN_SMALL_A_WITH_TILDE: u8 = 0xE3; // ã
const LATIN_SMALL_A_WITH_DIAERESIS: u8 = 0xE4; // ä
const LATIN_SMALL_A_WITH_RING_ABOVE: u8 = 0xE5; // å
const LATIN_SMALL_AE: u8 = 0xE6; // æ
const LATIN_SMALL_C_WITH_CEDILLA: u8 = 0xE7; // ç
const LATIN_SMALL_E_WITH_GRAVE: u8 = 0xE8; // è
const LATIN_SMALL_E_WITH_CUTE: u8 = 0xE9; // é
const LATIN_SMALL_E_WITH_CIRCUMFLEX: u8 = 0xEA; // ê
const LATIN_SMALL_E_WITH_DIAERESIS: u8 = 0xEB; // ë
const LATIN_SMALL_I_WITH_GRAVE: u8 = 0xEC; // ì
const LATIN_SMALL_I_WITH_CUTE: u8 = 0xED; // í
const LATIN_SMALL_I_WITH_CIRCUMFLEX: u8 = 0xEE; // î
const LATIN_SMALL_I_WITH_DIAERESIS: u8 = 0xEF; // ï
const LATIN_SMALL_ETH: u8 = 0xF0; // ð
const LATIN_SMALL_N_WITH_TILDE: u8 = 0xF1; // ñ
const LATIN_SMALL_O_WITH_GRAVE: u8 = 0xF2; // ò
const LATIN_SMALL_O_WITH_CUTE: u8 = 0xF3; // ó
const LATIN_SMALL_O_WITH_CIRCUMFLEX: u8 = 0xF4; // ô
const LATIN_SMALL_O_WITH_TILDE: u8 = 0xF5; // õ
const LATIN_SMALL_O_DIAERESIS: u8 = 0xF6; // ö
//  0xF7 is the division sign (÷)
const LATIN_SMALL_O_STROKE: u8 = 0xF8; // ø 
const LATIN_SMALL_U_WITH_GRAVE: u8 = 0xF9; // ù
const LATIN_SMALL_U_WITH_CUTE: u8 = 0xFA; // ú
const LATIN_SMALL_U_WITH_CIRCUMFLEX: u8 = 0xFB; // û
const LATIN_SMALL_U_WITH_DIAERESIS: u8 = 0xFC; // ü
const LATIN_SMALL_Y_WITH_CUTE: u8 = 0xFD; // ý
const LATIN_SMALL_THORN: u8 = 0xFE; // þ
const LATIN_SMALL_Y_WITH_DIAERESIS: u8 = 0xFF; // ÿ

/// Return the VHDL classification for the given Latin-1 byte.
pub fn kind_of(b: u8) -> CharKind {
    match b {
        0x00..=0x08 => CharKind::Invalid,
        TAB | LF | VT | FF | CR => CharKind::FormatEffector,
        0x0E..=0x1F => CharKind::Invalid,

        // Uppercase ASCII
        b'A'..=b'Z' => CharKind::UpperCaseLetter,
        // Uppercase Latin-1 (À..Ö and Ø..Þ)
        LATIN_CAPITAL_A_WITH_GRAVE..=LATIN_CAPITAL_O_DIAERESIS => CharKind::UpperCaseLetter,
        LATIN_CAPITAL_O_STROKE..=LATIN_CAPITAL_THORN => CharKind::UpperCaseLetter,

        // Digits
        b'0'..=b'9' => CharKind::Digit,

        // Special characters (as in the Ada source)
        b'"' | b'#' | b'&' | b'\'' | b'(' | b')' | b'+' | b',' | b'-' | b'.' | b'/'
        | b':' | b';' | b'<' | b'=' | b'>' | b'[' | b']' | b'_' | b'|' | b'*' => {
            CharKind::SpecialCharacter
        }

        // Space characters
        b' ' | NBSP => CharKind::SpaceCharacter,

        // Lowercase ASCII
        b'a'..=b'z' => CharKind::LowerCaseLetter,
        // Lowercase Latin-1 (ß..ö and ø..ÿ)
        LATIN_SMALL_SHARP_S..=LATIN_SMALL_O_DIAERESIS => CharKind::LowerCaseLetter,
        LATIN_SMALL_O_STROKE..=LATIN_SMALL_Y_WITH_DIAERESIS => CharKind::LowerCaseLetter,

        // Other special characters (partial set matching Ada comments)
        b'!' | b'$' | b'%' | b'@' | b'?' | b'\\' | b'^' | b'`' | b'{' | b'}' | b'~'
        | 0xA1..=0xBF | 0xD7 | 0xF7 => CharKind::OtherSpecialCharacter,

        // Everything else treated as Invalid
        0x7f..=0x9f => CharKind::Invalid,
    }
}

/// Return true iff the Latin-1 byte is a whitespace (space or format effector).
pub fn is_whitespace(b: u8) -> bool {
    matches!(b, b'\t' | b'\n' | VT | FF | b'\r' | b' ' | NBSP)
}

/// Convert an uppercase Latin-1 byte to its lowercase counterpart.
/// Bytes that are not uppercase are returned unchanged.
pub fn to_lower(b: u8) -> u8 {
    match b {
        // Uppercase ASCII
        b'A'..=b'Z' => b + 32,
        LATIN_SMALL_SHARP_S => LATIN_SMALL_SHARP_S, // ß has no uppercase
        // Uppercase Latin-1 (À..Ö and Ø..Þ)
        LATIN_CAPITAL_A_WITH_GRAVE..=LATIN_CAPITAL_O_DIAERESIS => b + 32,
        LATIN_CAPITAL_O_STROKE..=LATIN_CAPITAL_THORN => b + 32,
        LATIN_SMALL_Y_WITH_DIAERESIS => LATIN_SMALL_Y_WITH_DIAERESIS, // ÿ has no uppercase
        _ => b,
    }
}

/// Convert a Rust `&str` into a Latin-1 byte vector.
/// The string's characters must all be in the Latin-1 range (<= 0xFF).
pub fn to_iso8859_1(s: &str) -> Result<Vec<u8>, String> {
    let mut bytes = Vec::with_capacity(s.len());
    for ch in s.chars() {
        let code = ch as u32;
        if code > 0xFF {
            return Err(format!("non-Latin-1 character: U+{:04X}", code));
        }
        bytes.push(code as u8);
    }
    Ok(bytes)
}

/// Convert and validate an identifier in-place.
///
/// Returns `Ok(())` on success. On error, returns `Err(String)` with a message
/// similar to the original Ada `Error_Msg_Option` calls.
pub fn convert_identifier(s: &str, vhdl_std: VhdlStd) -> Result<Vec<u8>, String> {
    let mut chars: Vec<u8> = to_iso8859_1(s)?;

    if chars.is_empty() {
        return Err("identifier required".into());
    }

    if chars[0] == b'\\' {
        // Extended identifier
        if vhdl_std == VhdlStd::Vhdl87 {
            return Err("extended identifiers not allowed in vhdl87".into());
        }
        if chars.len() < 3 {
            return Err("extended identifier is too short".into());
        }
        if chars[chars.len() - 1] != b'\\' {
            return Err("extended identifier must finish with a '\\'".into());
        }
        // Check interior characters
        for i in 1..(chars.len() - 1) {
            let c = chars[i];
            match kind_of(c) {
                CharKind::FormatEffector => {
                    return Err("format effector in extended identifier".into())
                }
                CharKind::SpecialCharacter => {
                    if c == b'\\' {
                        // anti-slash must be doubled and can't be the last interior char
                        if chars.get(i + 1) != Some(&b'\\') || i == chars.len() - 2 {
                            return Err(
                                "anti-slash must be doubled in extended identifier".into(),
                            );
                        }
                    }
                }
                CharKind::Invalid => return Err("bad character in identifier".into()),
                _ => {}
            }
        }
    } else {
        // Regular identifier
        for i in 0..chars.len() {
            let c = chars[i];
            match kind_of(c) {
                CharKind::UpperCaseLetter => {
                    if vhdl_std == VhdlStd::Vhdl87 && c > b'Z' {
                        return Err("8 bits characters not allowed in vhdl87".into());
                    }
                    chars[i] = to_lower(c);
                }
                CharKind::LowerCaseLetter | CharKind::Digit => {
                    if vhdl_std == VhdlStd::Vhdl87 && c > b'z' {
                        return Err("8 bits characters not allowed in vhdl87".into());
                    }
                }
                CharKind::SpecialCharacter => {
                    if c == b'_' {
                        if i == 0 {
                            return Err("an identifier cannot start with an underscore".into());
                        }
                        if chars[i - 1] == b'_' {
                            return Err("two underscores can't be consecutive".into());
                        }
                        if i == chars.len() - 1 {
                            return Err("an identifier cannot finish with an underscore".into());
                        }
                    } else {
                        return Err("bad character in identifier".into());
                    }
                }
                _ => return Err("bad character in identifier".into()),
            }
        }
    }

    Ok(chars)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kind_and_case() {
        assert_eq!(kind_of(b'A'), CharKind::UpperCaseLetter);
        assert_eq!(kind_of(b'a'), CharKind::LowerCaseLetter);
        assert_eq!(to_lower(b'A'), b'a');
        assert!(is_whitespace(b' '));
    }
}
