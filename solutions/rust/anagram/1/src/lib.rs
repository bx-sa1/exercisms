use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let mut anagrams: HashSet<&'a str> = HashSet::new();
    
    let lower_word = word.to_lowercase();

    for possible_anagram in possible_anagrams.iter() {
        let lower_possible_anagram = possible_anagram.to_lowercase();

        if lower_possible_anagram.len() > lower_word.len() || lower_possible_anagram.len() < lower_word.len() { continue; }
        if lower_possible_anagram == lower_word { continue; }

        let mut temp_word = lower_word.clone();
        let mut is_anagram = true;
        for anagram_char in lower_possible_anagram.chars() {
            if !temp_word.contains(anagram_char) {
                is_anagram = false;
                break;
            } else {
                temp_word = temp_word.replacen(anagram_char, "", 1);
            }
        }

        if is_anagram {
            anagrams.insert(possible_anagram);
        }
    }

    anagrams
}
