pub fn is_letter(input: u8) -> bool {
    if (input >= b'a' && input <= b'z') || (input >= b'A' && input <= b'Z') || input == b'_' {
        return true;
    }
    return false;
}

pub fn is_digit(input: u8) -> bool {
    if input >= b'0' && input <= b'9' {
        return true;
    }
    return false;
}
