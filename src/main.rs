mod args;
mod compile;
mod core;
mod lexer;
mod parser;
mod program;
mod register_alloc;
mod semantic;
mod ssa;
mod util;

use std::{collections::BTreeMap, fs::read_to_string, mem::swap, ops::Range, process::exit};

use args::get_args;
use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::{Parser, input::Input, span::SimpleSpan};
use compile::{Register, compile_code, generate_asm};
use lexer::lexer;
use parser::{ParseNum, program_parser};
use program::Program;
use register_alloc::{LivelinessContainer, LivelinessGraph};
use semantic::Analyzed;
use ssa::FunctionIrGraph;

fn main() {
    let args = get_args();

    #[cfg(debug_assertions)]
    dbg!(&args);

    let content = read_to_string(&args.input_file).expect("could not read input file");

    let source = SourceFile {
        path: &args.input_file,
        content: &content,
    };

    let program = parse_file(source);

    let Some(program) = program else {
        exit(42);
    };

    #[cfg(debug_assertions)]
    dbg!(&program);

    let analyzed = analyze_program(program, source);

    let Some(analyzed) = analyzed else {
        println!("Semantic analyzer failed");
        exit(7);
    };

    dbg!(analyzed.structs);

    #[cfg(debug_assertions)]
    println!("Semantic analyzer passed");

    // let func_graphs =  analyzed.program.functions.into_iter().map(|func | FunctionIrGraph::new(func)).collect::<Vec<_>>();

    // let func_labels = BTreeMap::from_iter(func_graphs.iter().map(|func| (func.name, func.start_label)));

    // let mut assembly = Vec::new();

    // for func in &func_graphs {
    //     #[cfg(debug_assertions)]
    //     for block in func.graph.iter() {
    //         println!("{}", block);
    //     }

    //     let ir_graph = &func.graph;

    //     let live_container = LivelinessContainer::new(&ir_graph);

    //     let live_graph = LivelinessGraph::new(&ir_graph, &live_container);

    //     let registers = live_graph.greedy_coloring::<Register>();

    //     let mut asm = generate_asm(func, &registers, live_graph.visited_blocks(), &func_labels);

    //     assembly.append(&mut asm);
    // }

    // #[cfg(debug_assertions)]
    // for instr in &assembly {
    //     println!("{:?}", instr);
    // }

    // compile_code(args.output_file, assembly, true);

    // exit(0);
}

fn parse_file<'a>(source: SourceFile<'a>) -> Option<Program<'a, ParseNum<'a>>> {
    let (tokens, lex_errors) = lexer().parse(source.content).into_output_errors();

    let (program, parse_errors) = if let Some(tokens) = &tokens {
        program_parser()
            .parse(tokens.as_slice().map(
                (source.content.len()..source.content.len()).into(),
                |(t, s)| (t, s),
            ))
            .into_output_errors()
    } else {
        (None, Vec::new())
    };

    let lex_errors = lex_errors
        .into_iter()
        .map(|e| e.map_token(|t| t.to_string()));
    let parse_errors = parse_errors
        .into_iter()
        .map(|e| e.map_token(|t| t.to_string()));

    for error in lex_errors.chain(parse_errors) {
        let report = Report::build(ReportKind::Error, source.span(error.span()))
            .with_config(REPORT_CONFIG)
            .with_message(error.to_string())
            .with_label(
                Label::new(source.span(error.span()))
                    .with_message(error.reason().to_string())
                    .with_color(Color::Red)
                    .with_order(-1),
            )
            .with_labels(error.contexts().enumerate().map(|(idx, (label, span))| {
                Label::new(source.span(span))
                    .with_message(format!("while parsing {}", label))
                    .with_color(if idx == 0 { Color::Yellow } else { Color::Blue })
                    .with_order(idx as i32)
            }))
            .finish();

        report
            .print(sources([(source.path.to_string(), source.content)]))
            .unwrap();
    }

    program
}

fn analyze_program<'a>(
    program: Program<'a, ParseNum<'a>>,
    source: SourceFile<'a>,
) -> Option<Analyzed<'a>> {
    match Analyzed::new(program) {
        Ok(analyzed) => Some(analyzed),
        Err(errors) => {
            for error in errors {
                let report = Report::build(ReportKind::Error, source.span(&error.span()))
                    .with_config(REPORT_CONFIG)
                    .with_message(error.message())
                    .with_labels(error.labels(source))
                    .finish();

                report
                    .print(sources([(source.path.to_string(), source.content)]))
                    .unwrap();
            }

            None
        }
    }
}

const REPORT_CONFIG: ariadne::Config = ariadne::Config::new()
    .with_index_type(ariadne::IndexType::Byte)
    .with_label_attach(ariadne::LabelAttach::Start);

#[derive(Debug, Clone, Copy)]
struct SourceFile<'a> {
    path: &'a str,
    content: &'a str,
}

impl<'a> SourceFile<'a> {
    pub fn span(&self, span: &SimpleSpan) -> (String, Range<usize>) {
        let mut span = *span;
        if span.start > span.end {
            swap(&mut span.start, &mut span.end);
        }

        (self.path.to_string(), span.into_range())
    }
}
