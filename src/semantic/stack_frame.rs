use std::collections::HashMap;

pub struct StackFrame {
    pub base_offset: usize,      // Offset from fp for this frame
    pub current_offset: usize,   // Current offset relative to base
    pub locals: HashMap<String, isize>,     // Local variables offsets
    pub parameters: HashMap<String, usize>, // Parameters are in registers x0-x7 first
    pub alignment: usize,        // ARM64 typically uses 16-byte alignment
}



impl StackFrame {
    pub fn new(alignment: usize) -> Self {
        StackFrame {
            base_offset: 0,
            current_offset: 0,
            locals: HashMap::new(),
            parameters: HashMap::new(),
            alignment: 16,  // ARM64 requires 16-byte alignment
        }
    }


    // Modified to handle ARM64 stack layout
    pub fn allocate_local(&mut self, name: &str, size: usize) -> isize {
        // ARM64 grows down, so we need negative offsets from fp
        let aligned_size = crate::semantic::symbol_table::align_to(size, self.alignment);
        self.current_offset += aligned_size;
        let offset = -(self.current_offset as isize);
        self.locals.insert(name.to_string(), offset);
        offset
    }


    // New method for parameter allocation
    pub fn allocate_parameter(&mut self, name: &str, param_index: usize) {
        if param_index < 8 {
            // First 8 parameters go in registers x0-x7
            self.parameters.insert(name.to_string(), param_index);
        } else {
            // Additional parameters are on stack at fp+16 and above
            let stack_offset = 16 + ((param_index - 8) * 8);
            self.parameters.insert(name.to_string(), stack_offset);
        }
    }
    

    pub fn get_frame_size(&self) -> usize {
        crate::semantic::symbol_table::align_to(self.current_offset, self.alignment)
    }

    pub fn print(&self) {
        println!("\n--- ARM64 Stack Frame Layout ---");
        println!("Stack grows downward. Offsets relative to fp (frame pointer)");

        println!("\nParameter Registers:");
        let mut reg_params: Vec<_> = self.parameters.iter()
            .filter(|(_, idx)| **idx < 8)  // Fixed line
            .collect();
        reg_params.sort_by_key(|(_, idx)| *idx);

        if reg_params.is_empty() {
            println!("  (none)");
        } else {
            for (name, reg_idx) in reg_params {
                println!("  x{:<2} : {}", reg_idx, name);
            }
        }

        println!("\nStack Parameters:");
        let mut stack_params: Vec<_> = self.parameters.iter()
            .filter(|(_, idx)| **idx >= 8)  // Fixed line
            .collect();
        stack_params.sort_by_key(|(_, offset)| *offset);
        if stack_params.is_empty() {
            println!("  (none)");
        } else {
            for (name, offset) in stack_params {
                println!("  fp+{:<4} : {}", offset, name);
            }
        }

        println!("\nFrame Layout:");
        println!("  fp+16  : (stack parameters if any)");
        println!("  fp+8   : saved lr (link register)");
        println!("  fp+0   : saved fp");

        println!("\nLocal Variables (lower addresses):");
        if self.locals.is_empty() {
            println!("  (none)");
        } else {
            let mut locals: Vec<_> = self.locals.iter().collect();
            locals.sort_by_key(|(_, offset)| -**offset);

            for (name, offset) in locals {
                println!("  fp{:<5} : {}", offset, name);
            }
        }

        println!("\nCurrent sp      --> {}", -(self.current_offset as isize));

        println!("\nFrame Statistics:");
        println!("  Total frame size: {} bytes", self.get_frame_size());
        println!("  Local variables space: {} bytes", self.current_offset);
        let stack_params_count = self.parameters.values().filter(|idx| **idx >= 8).count();
        println!("  Stack parameters space: {} bytes", stack_params_count * 8);
        println!("  Register parameters: {}", self.parameters.values().filter(|idx| **idx < 8).count());
        println!("--- End ARM64 Stack Frame Layout ---\n");
    }
}




