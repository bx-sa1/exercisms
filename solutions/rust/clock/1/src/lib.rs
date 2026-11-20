use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub struct Clock(i32);

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let h = hours % 24;
        let m = minutes % 60;
        Self(h* 3600 + m * 60)
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        let m = minutes % 60;
        let s = self.0 + m * 60;
        Self(s)
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut hour = self.0 / 3600 % 24;
        let mut minute = self.0 % 3600 / 60 % 60;
        if hour < 0 {
            hour = 24 + hour;
        } else if minute < 0 {
            minute = 60 + minute;
        }
        write!(f, "{:02}:{:02}", hour, minute)
    }
}
