use std::{str::FromStr, convert::TryFrom, slice::Iter};

pub type Value = i32;
pub type Result<T> = std::result::Result<T, Error>;
pub type UnitResult = Result<()>;
pub type Stack = Vec<Value>;

pub struct Forth {
    stack: Stack,
    user_words: Vec<UserWord>
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

pub struct UserWord {
    name: String,
    body: Vec<Word>
}

#[derive(Clone)]
pub enum Word {
    Add,
    Sub,
    Mul,
    Div,
    Dup,
    Drop,
    Swap,
    Over,
    Number(Value),
    Call(Value)
}

impl TryFrom<&str> for Word {

    type Error = Error;

    fn try_from(value: &str) -> std::prelude::v1::Result<Self, Self::Error> {
        match value {
            "+" => Ok(Word::Add),
            "-" => Ok(Word::Sub),
            "*" => Ok(Word::Mul),
            "/" => Ok(Word::Div),
            "DUP" => Ok(Word::Dup),
            "DROP" => Ok(Word::Drop),
            "SWAP" => Ok(Word::Swap),
            "OVER" => Ok(Word::Over),
            _ => if let Ok(num) = Value::from_str(value) {
                Ok(Word::Number(num))
            } else {
                Err(Error::UnknownWord)
            }
        }
    }
}

impl Forth {
    pub fn new() -> Forth {
       Forth {
           stack: Vec::new(),
           user_words: Vec::new()
       }
    }

    pub fn stack(&self) -> &[Value] {
        self.stack.as_slice()
    }

    pub fn eval(&mut self, input: &str) -> UnitResult {
        let mut iter = input.split_ascii_whitespace();
        while let Some(word) = iter.next() {
            self.parse_word(word, &mut iter)?;
        }

        Ok(())
    }

    pub fn parse_word<'a>(&mut self, word: &'a str, iter: &mut impl Iterator<Item = &'a str>) -> UnitResult {
        if word == ":" {
            self.parse_definition(iter)?;
        } else {
            let word = self.parse_normal_word(word)?;
            self.execute(word)?;
        }

         Ok(())
    }

    pub fn parse_normal_word(&mut self, word: &str) -> Result<Word> {
        let canonical = word.to_ascii_uppercase();
        if let Some(call) = self.find_user_word(&canonical) {
            Ok(call)
        } else {
            match Word::try_from(canonical.as_ref()) {
                Ok(o) => Ok(o),
                Err(_) => return Err(Error::UnknownWord)
            }
        }
    }
    
    pub fn parse_definition<'a>(&mut self, iter: &mut impl Iterator<Item = &'a str>) -> UnitResult {
        if let Some(new_word) = iter.next() {
            if let Ok(_) = Value::from_str(new_word) {
                return Err(Error::InvalidWord);
            }
            let name = new_word.to_ascii_uppercase();
            let mut body = Vec::new();
            for word in iter {
                if word == ";" {
                    self.user_words.push(UserWord {name, body});
                    return Ok(());
                } else {
                    body.push(self.parse_normal_word(word)?);
                }
            }
        }

        Err(Error::InvalidWord)
    }

    pub fn find_user_word(&mut self, word: &str) -> Option<Word> {
        for (i, u) in self.user_words.iter().enumerate().rev() {
            if u.name == word {
                return Some(Word::Call(i.try_into().unwrap()));
            }
        }
        None
    }

    pub fn execute(&mut self, word: Word) -> UnitResult {
        match word {
            Word::Add => self.arithmetic(|a, b| Ok(a + b)),
            Word::Sub => self.arithmetic(|a, b| Ok(a - b)),
            Word::Mul => self.arithmetic(|a, b| Ok(a * b)),
            Word::Div => self.arithmetic(|a, b| if b == 0 {
                Err(Error::DivisionByZero)
            } else {
                Ok(a / b)
            }),

            Word::Dup => self.dup(),
            Word::Drop => self.drop(),
            Word::Swap => self.swap(),
            Word::Over => self.over(),

            Word::Number(n) => { self.push(n); Ok(()) },
            Word::Call(id) => self.call(id)
        }
    }

    pub fn push(&mut self, v: Value) {
        self.stack.push(v);
    }

    pub fn pop(&mut self) -> Result<Value> {
        match self.stack.pop() {
           Some(e) => Ok(e),
           None => Err(Error::StackUnderflow)
        }
    }

    pub fn arithmetic<F>(&mut self, func: F) -> UnitResult
        where F: Fn(Value, Value) -> Result<Value> {
        let a = self.pop()?;
        let b = self.pop()?;

        self.push(func(b, a)?);
        Ok(())
    }

    pub fn dup(&mut self) -> UnitResult { 
        let v = self.pop()?;
        self.push(v);
        self.push(v);
        Ok(())
    }

    pub fn drop(&mut self) -> UnitResult {
        self.pop()?;
        Ok(())
    }

    pub fn swap(&mut self) -> UnitResult {
        let a = self.pop()?;
        let b = self.pop()?;
        self.push(a);
        self.push(b);
        Ok(())
    }

    pub fn over(&mut self) -> UnitResult {
        let a = self.pop()?;
        let b = self.pop()?;
        self.push(b);
        self.push(a);
        self.push(b);
        Ok(())
    }

    pub fn call(&mut self, id: Value) -> UnitResult {
        if id > self.user_words.len().try_into().unwrap() {
            Err(Error::InvalidWord)
        } else {
            let user_word = self.user_words[id as usize].body.clone();
            for word in user_word {
                self.execute(word)?;
            }
            Ok(())
        }
    }
}
