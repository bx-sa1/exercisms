use std::{collections::HashMap, convert::TryFrom};

pub type Value = i32;
pub type Result = std::result::Result<(), Error>;
pub type Stack = Vec<Value>;

pub struct Forth {
    stack: Stack,
    user_words: HashMap<String, (Value, String)>
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}



impl Forth {
    pub fn new() -> Forth {
       Forth {
           stack: Stack::new(),
           user_words: HashMap::new()
       }
    }

    pub fn stack(&self) -> &[Value] {
        self.stack.as_slice()
    }

    pub fn eval(&mut self, input: &str) -> Result {
        self.decode(input)?;
        self.execute()
    }

    pub fn decode(&mut self, input: &str) -> Result {
        let mut operator_stack: Vec<Value> = Vec::new();

        let mut current_word: String = String::new();
        let mut current_word_def: String = String::new();
        let mut word_def_count = 0;
        let mut chars = input.chars();
        while let Some(c) = chars.next() {
            if c == ' ' {
                if current_word.len() != 0 {
                    if current_word == ":" {
                        while let Some(def_char) = chars.next() {
                            if def_char == ';' {
                                break;
                            }
                            current_word_def.push(def_char);
                        }

                        let def: Vec<&str> = current_word_def.splitn(1, ' ').collect();
                        self.user_words.insert(def[0].to_string(), (word_def_count, def[1].to_string()));
                        word_def_count += 1;
                    } else if current_word.chars().all(|x| x.is_numeric()) { //integer
                        self.stack.push(current_word.parse().unwrap());
                    } else {
                        match current_word.to_lowercase().as_str() {
                            "+" => operator_stack.push(Word::ADD as Value),
                            "-" => operator_stack.push(Word::MIN as Value),
                            "*" => operator_stack.push(Word::MUL as Value),
                            "/" => operator_stack.push(Word::DIV as Value),
                            "dup" => operator_stack.push(Word::DUP as Value),
                            "drop" => operator_stack.push(Word::DROP as Value),
                            "swap" => operator_stack.push(Word::SWAP as Value),
                            "over" => operator_stack.push(Word::OVER as Value),
                            user if self.user_words.contains_key(user) => {
                                let u = self.user_words.get(user).unwrap();
                                operator_stack.push(Word::USER_END as Value);
                                operator_stack.push(u.0);
                                operator_stack.push(Word::USER as Value);
                            },
                            _ => return Err(Error::UnknownWord)
                        }
                    }

                    operator_stack.sort();
                    self.stack.append(&mut operator_stack);
                }
            } else {
                current_word.push(c);
            }
        }

        Ok(())
    }

    pub fn execute(&mut self) -> Result {
        while let Some(value) = self.stack.pop() {
            let word = match Word::try_from(value) {
                Ok(ok) => ok,
                Err(_) => continue
            };
            
            if word.clone() as i32 == Word::USER_END as i32 {
                break;
            }

            self.exec_word(word)?;
        }

        Ok(())
    }

    pub fn exec_word(&mut self, word: Word) -> Result {
        match word {
            Word::ADD => {
                let a = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                let b = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                self.exec_word(Word::try_from(a)?);
                self.exec_word(Word::try_from(b)?);
                self.stack.push(a + b);
            },
            Word::MIN => {
                let a = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                let b = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                self.exec_word(Word::try_from(a)?);
                self.exec_word(Word::try_from(b)?);
                self.stack.push(a - b);
            },
            Word::MUL => {
                let a = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                let b = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                self.exec_word(Word::try_from(a)?);
                self.exec_word(Word::try_from(b)?);
                self.stack.push(a * b);
            },
            Word::DIV => {
                let a = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                let b = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                self.exec_word(Word::try_from(a)?);
                self.exec_word(Word::try_from(b)?);
                if b == 0 {
                    return Err(Error::DivisionByZero);
                } else {
                    self.stack.push(a / b);
                }
            },
            Word::DUP => {
                let last = match self.stack.last().copied() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };
                self.stack.push(last);
            },
            Word::DROP => {
                self.stack.pop();
            },
            Word::SWAP => {
                let a = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                let b = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                self.stack.push(a);
                self.stack.push(b);
            },
            Word::OVER => {
                let a = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                let b = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                self.stack.push(b);
                self.stack.push(a);
                self.stack.push(b);
            },
            Word::USER => {
                let a = match self.stack.pop() {
                    Some(x) => x,
                    None => return Err(Error::StackUnderflow)
                };

                let code = match self.user_words.iter()
                    .find(|e| e.1.0 == a) {
                        Some((_, v)) => &v.1,
                        None => return Err(Error::InvalidWord)
                    };

                self.decode(&code);
                self.execute();
            },
            Word::USER_END => {}
        };
        Ok(())
    }
}


#[derive(Clone)]
pub enum Word {
    DUP = 0xff00,
    DROP,
    SWAP,
    OVER,
    DIV,
    MUL,
    ADD,
    MIN,
    USER_END,
    USER
}

impl TryFrom<Value> for Word {
    type Error = Error;

    fn try_from(v: Value) -> std::result::Result<Self, Self::Error> {
        match v {
            x if x == Word::DUP as Value => Ok(Word::DUP),
            x if x == Word::DROP as Value => Ok(Word::DROP),
            x if x == Word::SWAP as Value => Ok(Word::SWAP),
            x if x == Word::OVER as Value => Ok(Word::OVER),
            x if x == Word::DIV as Value => Ok(Word::DIV),
            x if x == Word::MUL as Value => Ok(Word::MUL),
            x if x == Word::ADD as Value => Ok(Word::ADD),
            x if x == Word::MIN as Value => Ok(Word::MIN),
            x if x == Word::USER as Value => Ok(Word::USER),
            x if x == Word::USER_END as Value => Ok(Word::USER_END),
            _ => Err(Error::UnknownWord)
        }
    }
}
