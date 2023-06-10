use regex::Regex;
use std::{
    fmt::{self, Display},
    str::FromStr,
};

/// Represents a row / column coordinate on the [Grid].
#[derive(Debug, PartialEq)]
pub struct Location(pub u8, pub u8);

impl Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self:?}")
    }
}
/// The [Location] could not be parsed from the string.
#[derive(Debug, Clone)]
pub struct LocationParseStrError {
    loc: String,
}

impl Display for LocationParseStrError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Location cannot be parsed from {}. Locations must be of the format [a-c][1-3], e.g. a1", self.loc)
    }
}

impl FromStr for Location {
    type Err = LocationParseStrError;

    fn from_str(loc: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"^([a-c])([1-3])$").map_err(|_| LocationParseStrError {
            loc: loc.to_string(),
        })?;
        let loc = loc.to_lowercase();
        let text = loc.trim();
        let captures = re.captures(text).ok_or(LocationParseStrError {
            loc: loc.to_string(),
        })?;

        let column = captures
            .get(1)
            .ok_or(LocationParseStrError {
                loc: loc.to_string(),
            })?
            .as_str();
        let row = captures
            .get(2)
            .ok_or(LocationParseStrError {
                loc: loc.to_string(),
            })?
            .as_str();

        let column = match column {
            "a" => 0,
            "b" => 1,
            "c" => 2,
            _ => unreachable!(),
        };
        let row = row.parse::<u8>().map_err(|_| LocationParseStrError {
            loc: loc.to_string(),
        })? - 1;

        Ok(Self(column, row))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_location_from_str() {
        assert_eq!("A1".parse::<Location>().unwrap(), Location(0, 0));
        assert_eq!("B1".parse::<Location>().unwrap(), Location(1, 0));
        assert_eq!("C1".parse::<Location>().unwrap(), Location(2, 0));
        assert_eq!("A2".parse::<Location>().unwrap(), Location(0, 1));
        assert_eq!("B2".parse::<Location>().unwrap(), Location(1, 1));
        assert_eq!("C2".parse::<Location>().unwrap(), Location(2, 1));
        assert_eq!("A3".parse::<Location>().unwrap(), Location(0, 2));
        assert_eq!("B3".parse::<Location>().unwrap(), Location(1, 2));
        assert_eq!("C3".parse::<Location>().unwrap(), Location(2, 2));
    }

    #[test]
    fn test_location_from_almost_valid_input() {
        assert!("B22222".parse::<Location>().is_err());
        assert!("AA2".parse::<Location>().is_err());
    }

    #[test]
    fn test_location_from_invalid_column() {
        assert!("D1".parse::<Location>().is_err())
    }

    #[test]
    fn test_location_from_invalid_row() {
        assert!("C4".parse::<Location>().is_err());
    }
}
