use std::fmt::Display;

const DAY: i32 = 24 * HOUR;
const HOUR: i32 = 60;

#[derive(Debug, PartialEq, Eq)]
pub struct Clock(i32);

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Self((((hours * HOUR + minutes) % DAY) + DAY ) % DAY)
    }

    pub fn add_minutes(self, minutes: i32) -> Self {
        Clock::new(0, self.0 + minutes)
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02}:{:02}", self.0 / 60, self.0 % 60)
    }
}
