use super::SsaInstruction;

pub fn remove_dead_code<'a>(instructions: Vec<SsaInstruction<'a>>) -> Vec<SsaInstruction<'a>> {
    let mut after_return = false;

    let instructions = instructions
        .into_iter()
        .map(|instr| {
            let status = if after_return {
                Status::Dead
            } else {
                Status::Alive
            };

            match &instr {
                SsaInstruction::Return { .. } => after_return = true,
                _ => (),
            }

            (instr, status)
        })
        .filter_map(|(instr, status)| match status {
            Status::Alive => Some(instr),
            Status::Dead => None,
        })
        .collect();

    instructions
}

enum Status {
    Alive,
    Dead,
}
