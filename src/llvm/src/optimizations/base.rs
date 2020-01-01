use crate::instructions as LLVM;

pub trait Optimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program;
}

pub fn apply_optimizers(prog: &LLVM::Program, opts: &mut Vec<Box<dyn Optimizer>>, max_runs: i32) -> (LLVM::Program, i32) {
    let mut runs = 0;
    let mut new_prog: LLVM::Program = prog.clone();

    while runs < max_runs {
        runs += 1;

        let old_prog = new_prog.clone();
        for i in 0..opts.len() {
            new_prog = opts[i].run(&new_prog);
        }

        if old_prog == new_prog {
            break
        } else if runs >= max_runs {
            println!("WARNING: optimizations stopped after reaching maximal executions count!");
            break;
        }
    }

    (new_prog, runs)
}
