use std::{env, ffi, fs, io, iter, os};
mod errorout_def;
mod errorout;
mod vhdl;

#[derive(Clone, Copy)]
#[repr(u8)]
enum VhdlStd {
    Vhdl87,
    Vhdl93,
    Vhdl00,
    Vhdl02,
    Vhdl08,
    Vhdl19,
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq)]
struct NameId {
    v: u32,
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq)]
struct VhdlNode {
    v: u32,
}
const NULL_VHDLNODE: VhdlNode = VhdlNode { v: 0 };

extern "C" {
    #[link_name = "flags__vhdl_std"]
    static mut vhdl_std: VhdlStd;

    #[link_name = "flags__flag_relaxed_rules"]
    static mut flag_relaxed: bool;

    #[link_name = "flags__flag_elaborate_with_outdated"]
    static mut flag_elaborate_with_outdated: bool;

    #[link_name = "flags__flag_only_elab_warnings"]
    static mut flag_only_elab_warnings: bool;

    #[link_name = "flags__flag_syn_binding"]
    static mut flag_syn_binding: bool;

    #[link_name = "flags__flag_synopsys"]
    static mut flag_synopsys: bool;

    #[link_name = "vhdl__scanner__flag_psl_comment"]
    static mut flag_psl_comment: bool;

    #[link_name = "vhdl__scanner__flag_comment_keyword"]
    static mut flag_comment_keyword: bool;

    #[link_name = "grt__options__progname"]
    static mut options_progname: *const os::raw::c_char;

    #[link_name = "grt__options__argc"]
    static mut options_argc: u32;

    #[link_name = "grt__options__argv"]
    static mut options_argv: *const *const os::raw::c_char;

    #[link_name = "options__initialize"]
    fn options_initialize();

    #[link_name = "name_table__get_identifier_with_len"]
    fn get_identifier_with_len(s: *const u8, len: u32) -> NameId;

    #[link_name = "name_table__get_name_length"]
    fn get_name_length(id: NameId) -> u32;

    #[link_name = "name_table__get_name_ptr"]
    fn get_name_ptr(id: NameId) -> *const u8;

    #[link_name = "ghdlcomp__compile_elab_top"]
    fn compile_elab_top(
        lib: NameId,
        prim: NameId,
        sec: NameId,
        allow_undefined_generics: bool,
    ) -> VhdlNode;

    #[link_name = "ghdlrun__compile_init"]
    fn compile_init(analyze_only: bool);

    #[link_name = "ghdlrun__compile_elab_setup"]
    fn compile_elab_setup(config: VhdlNode);

    #[link_name = "ghdlrun__run"]
    fn run();

    #[link_name = "ghdlcomp__analyze_file"]
    fn analyze_file(id: NameId) -> bool;

    //  From errorout-console
    #[link_name = "errorout__console__set_program_name_with_len"]
    fn set_program_name(s: *const u8, len: u32);

    #[link_name = "errorout__console__install_handler"]
    fn errorout_console_install_handler();

    #[link_name = "errorout__warning_error"]
    fn warning_error(id: u8, as_error: bool);

    #[link_name = "errorout__enable_warning"]
    fn enable_warning(id: u8, as_error: bool);

    #[link_name = "libraries__work_directory"]
    static mut work_directory: NameId;

    #[link_name = "libraries__work_library"]
    static mut work_library: VhdlNode;

    #[link_name = "libraries__work_library_name"]
    static mut work_library_name: NameId;

    #[link_name = "libraries__load_work_library"]
    fn load_work_library(empty: bool);

    #[link_name = "libraries__save_work_library"]
    fn save_work_library();

    #[link_name = "vhdl__nodes__get_identifier"]
    fn get_identifier(n: VhdlNode) -> NameId;

    //  From binder file
    #[link_name = "ghdl_rust_init"]
    fn ghdl_rust_init();
}

impl NameId {
    const NULL: NameId = NameId { v: 0 };

    fn from_string(s: &str) -> NameId {
        unsafe { get_identifier_with_len(s.as_ptr(), s.len() as u32) }
    }

    fn to_string(&self) -> String {
        let len = unsafe { get_name_length(*self) };
        let ptr = unsafe { get_name_ptr(*self) };
        let mut res = String::with_capacity(len as usize);

        //  FIXME: is it the most canonical way ?
        for i in 0..len {
            res.push(unsafe { *ptr.add(i as usize) as char });
        }
        res
    }
}

impl VhdlNode {
    fn get_identifier(&self) -> NameId {
        unsafe { get_identifier(*self) }
    }
}
fn library_to_filename(lib: VhdlNode, std: VhdlStd) -> String {
    let mut res = lib.get_identifier().to_string();

    res.push_str("-obj");
    match std {
        VhdlStd::Vhdl87 => {
            res.push_str("87");
        }
        VhdlStd::Vhdl93 | VhdlStd::Vhdl00 | VhdlStd::Vhdl02 => {
            res.push_str("93");
        }
        VhdlStd::Vhdl08 => {
            res.push_str("08");
        }
        VhdlStd::Vhdl19 => {
            res.push_str("19");
        }
    }
    res.push_str(".cf");
    res
}

#[derive(Clone, Copy)]
enum WarnValue {
    Default,
    Enable,
    Disable,
}

#[derive(Clone, Copy)]
struct WarnState {
    enable: WarnValue,
    error: WarnValue,
}

#[derive(Clone, Copy)]
struct VhdlAnalyzeFlags {
    std: VhdlStd,
    relaxed: bool,
    bootstrap: bool,
    synopsys_pkgs: bool,
    synth_binding: bool,
    work_name: NameId,
    work_dir: NameId,
    psl_comment: bool,
    comment_keyword: bool,
    warnings: [WarnState; crate::errorout::WARNID_USIZE],
}

impl Default for VhdlAnalyzeFlags {
    fn default() -> Self {
        Self {
         std: VhdlStd::Vhdl93,
         relaxed: true,
        bootstrap: false,
        synopsys_pkgs: false,
        synth_binding: false,
        work_name: NameId::NULL,
        work_dir: NameId::NULL,
        psl_comment: false,
        comment_keyword: false,
        warnings: [WarnState {enable: WarnValue::Default, error: WarnValue::Default}; crate::errorout::WARNID_USIZE],
        }
    }
}

fn apply_analyze_flags(flags: &VhdlAnalyzeFlags) {
    unsafe {
        vhdl_std = flags.std;
        flag_relaxed = flags.relaxed;
        flag_comment_keyword = flags.comment_keyword;
        flag_psl_comment = flags.psl_comment;
        flag_syn_binding = flags.synth_binding;
        flag_synopsys = flags.synopsys_pkgs;
        if flags.work_name != NameId::NULL {
            work_library_name = flags.work_name;
        }
        if flags.work_dir != NameId::NULL {
            work_directory = flags.work_dir;
        }
        for (i, w) in flags.warnings.iter().enumerate() {
            let id = crate::errorout::MSGID_FIRST_WARNID + (i as u8);
            match w.error {
                WarnValue::Default => {},
                WarnValue::Enable => { warning_error(id, true); },
                WarnValue::Disable => { warning_error(id, false); },
            }
            match w.enable {
                WarnValue::Default => {},
                WarnValue::Enable => { enable_warning(id, true); },
                WarnValue::Disable => {enable_warning(id, false); },
            }
        }
    }
}

/// Return true if arg is an option (starts with '-')
fn is_option(arg: &str) -> bool {
    arg.chars().nth(0).unwrap() == '-'
}

// Command line option parsing status
enum ParseStatus {
    // Command is unknown
    UnknownCommand,

    // The option is unknown
    UnknownOption { msg: String },

    // The option is known, but it has an error (invalid value)
    OptionError,

    // The option is not an option (doesn't start with '-')
    NotOption,

    // Command failed
    CommandError,

    // Command failed as expected
    CommandExpectedError,
}

//  Try to parse an analysis flag and return None if ok or the error.
fn parse_analyze_flags(flags: &mut VhdlAnalyzeFlags, arg: &str) -> Option<ParseStatus> {
    if !is_option(arg) {
        return Some(ParseStatus::NotOption);
    }
    if arg.get(0..6) == Some("--std=") {
        let (_, val) = arg.split_at(6);
        flags.relaxed = false;
        match val {
            "87" => flags.std = VhdlStd::Vhdl87,
            "93" => flags.std = VhdlStd::Vhdl93,
            "93c" => {
                flags.std = VhdlStd::Vhdl93;
                flags.relaxed = true;
            }
            "00" => flags.std = VhdlStd::Vhdl00,
            "02" => flags.std = VhdlStd::Vhdl02,
            "08" => flags.std = VhdlStd::Vhdl08,
            "19" => flags.std = VhdlStd::Vhdl19,
            _ => return Some(ParseStatus::OptionError),
        }
        return None;
    }
    if arg == "--bootstrap" {
        flags.bootstrap = true;
        return None;
    }
    if arg == "-fsynopsys" || arg == "--ieee=synopsys" {
        flags.synopsys_pkgs = true;
        return None;
    }
    if arg == "-frelaxed" || arg == "-frelaxed-rules" {
        flags.relaxed = true;
        return None;
    }
    if arg == "-fpsl" {
        flags.comment_keyword = true;
        flags.psl_comment = true;
        return None;
    }
    if arg == "--syn-binding" {
        flags.synth_binding = true;
        return None;
    }
    if arg.starts_with("--work=") {
        flags.work_name = NameId::from_string(&arg[7..]);
        return None;
    }
    if arg.starts_with("--workdir=") {
        let val = &arg[10..];
        const SEP: char = std::path::MAIN_SEPARATOR;
        if val == "." {
            flags.work_dir = NameId::NULL;
        } else if val.chars().last().unwrap() == SEP {
            flags.work_dir = NameId::from_string(val);
        } else {
            let mut dir = String::with_capacity(val.len() + 1);
            dir.push_str(val);
            dir.push(SEP);
            flags.work_dir = NameId::from_string(&dir);
        }
        return None;
    }
    if arg == "-g" || arg == "-O" {
        //  TODO: for the back-end.
        return None;
    }
    if arg == "-Werror" {
        for w in flags.warnings.iter_mut() {
            w.error = WarnValue::Enable;
        }
        return None;
    }
    return Some(ParseStatus::UnknownOption {
        msg: arg.to_string(),
    });
}

fn analyze(args: &[String], save: bool) -> Result<(), ParseStatus> {
    let mut status = true;
    let mut flags = VhdlAnalyzeFlags::default();
    let mut expect_failure = false;
    let mut files = vec![];

    // Parse arguments
    for arg in &args[1..] {
        if arg == "--expect-failure" {
            expect_failure = true;
        } else {
            match parse_analyze_flags(&mut flags, &arg) {
                None => {}
                Some(ParseStatus::NotOption) => files.push(arg.clone()),
                Some(err) => return Err(err),
            }
        }
    }

    // Initialize
    apply_analyze_flags(&flags);
    unsafe {
        compile_init(true);
    };

    // And analyze every file
    for file in &files {
        let id = unsafe { get_identifier_with_len(file.as_ptr(), file.len() as u32) };
        eprintln!("analyze {file}\n");
        status = unsafe { analyze_file(id) };
        if !status {
            break;
        }
    }

    // Save the library on success
    if status && save {
        unsafe { save_work_library() };
    }
    return if status == !expect_failure {
        Ok(())
    } else {
        Err(ParseStatus::OptionError)
    };
}

trait Command {
    fn get_command(&self) -> &'static [&'static str];
    fn execute(&self, args: &[String]) -> Result<(), ParseStatus>;
}

struct CommandAnalyze {}

impl Command for CommandAnalyze {
    fn get_command(&self) -> &'static [&'static str] {
        return &["analyze", "-a"];
    }

    fn execute(&self, args: &[String]) -> Result<(), ParseStatus> {
        analyze(args, true)
    }
}

struct CommandSyntax {}

impl Command for CommandSyntax {
    fn get_command(&self) -> &'static [&'static str] {
        return &["syntax", "-s"];
    }

    fn execute(&self, args: &[String]) -> Result<(), ParseStatus> {
        analyze(args, false)
    }
}

struct CommandImport {}

impl Command for CommandImport {
    fn get_command(&self) -> &'static [&'static str] {
        return &["import", "-i"];
    }

    fn execute(&self, args: &[String]) -> Result<(), ParseStatus> {
        let mut status = true;
        let mut flags = VhdlAnalyzeFlags::default();
        let mut expect_failure = false;
        let mut files = vec![];
        let mut got_file = false;

        // Parse arguments
        for arg in &args[1..] {
            if got_file {
                files.push(arg.clone());
            }
            else if arg == "--expect-failure" {
                expect_failure = true;
            } else {
                match parse_analyze_flags(&mut flags, &arg) {
                    None => {}
                    Some(ParseStatus::NotOption) => {got_file = true; files.push(arg.clone()); },
                    Some(err) => return Err(err),
                }
            }
        }

        // Initialize
        apply_analyze_flags(&flags);
        unsafe {
            compile_init(true);
        };

        // And analyze every file
        for file in &files {
            if file.starts_with("--work=") {

            }
            let id = unsafe { get_identifier_with_len(file.as_ptr(), file.len() as u32) };
            status = unsafe { analyze_file(id) };
            if !status {
                break;
            }
        }

        // Save the library on success
        if status {
            unsafe { save_work_library() };
        }
        return if status == !expect_failure {
            Ok(())
        } else {
            Err(ParseStatus::OptionError)
        };
    }
}

struct CommandRemove {}

impl Command for CommandRemove {
    fn get_command(&self) -> &'static [&'static str] {
        return &["--remove"];
    }

    fn execute(&self, args: &[String]) -> Result<(), ParseStatus> {
        let mut flags = VhdlAnalyzeFlags::default();

        for arg in &args[1..] {
            match parse_analyze_flags(&mut flags, &arg) {
                None => {}
                Some(err) => return Err(err),
            }
        }
        apply_analyze_flags(&flags);
        unsafe {
            compile_init(true);
        };
        let mut lib_name = unsafe { work_directory.to_string() };
        let file_name = unsafe { library_to_filename(work_library, flags.std) };
        lib_name.push_str(&file_name);
        match fs::remove_file(lib_name) {
            Ok(()) => {}
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => {}
                _ => {
                    eprintln!("cannot remove '{}': {}", file_name, err)
                }
            },
        }
        Ok(())
    }
}

fn analyze_elab(args: &[String]) -> Result<Vec<String>, ParseStatus> {
    let mut flags = VhdlAnalyzeFlags::default();
    let mut expect_failure = false;
    let mut unit = NameId::NULL;
    let mut arch = NameId::NULL;
    let mut runflags = vec![];

    // Parse arguments
    let mut has_unit = false;
    for arg in &args[1..] {
        if is_option(&arg) {
            if has_unit {
                runflags.push(arg.clone());
            } else if arg == "--expect-failure" {
                expect_failure = true;
            } else if let Some(err) = parse_analyze_flags(&mut flags, &arg) {
                return Err(err);
            }
        } else {
            if unit == NameId::NULL {
                unit = NameId::from_string(arg);
            } else if arch == NameId::NULL {
                arch = NameId::from_string(arg);
            } else {
                eprintln!("too many unit names");
                return Err(ParseStatus::OptionError);
            }
            has_unit = true;
        }
    }
    apply_analyze_flags(&flags);
    unsafe {
        compile_init(false);
        load_work_library(false);
        flag_elaborate_with_outdated = false;
        flag_only_elab_warnings = true;
    };
    let top = unsafe { compile_elab_top(NameId::NULL, unit, arch, false) };
    if top == NULL_VHDLNODE {
        if expect_failure {
            return Result::Err(ParseStatus::CommandExpectedError);
        }
        eprintln!("Failed to build top");
        return Result::Err(ParseStatus::OptionError);
    }
    unsafe {
        compile_elab_setup(top);
    }
    if expect_failure {
        return Result::Err(ParseStatus::CommandError);
    }
    return Ok(runflags);
}

struct CommandElab {}

impl Command for CommandElab {
    fn get_command(&self) -> &'static [&'static str] {
        return &["-e", "elab", "--elab"];
    }

    fn execute(&self, args: &[String]) -> Result<(), ParseStatus> {
        match analyze_elab(args) {
            Ok(_) => {}
            Err(err) => return Err(err),
        }
        Ok(())
    }
}

struct CommandRun {}

impl Command for CommandRun {
    fn get_command(&self) -> &'static [&'static str] {
        return &["-r", "run", "--elab-run"];
    }

    fn execute(&self, args: &[String]) -> Result<(), ParseStatus> {
        let runflags: Vec<String>;

        match analyze_elab(args) {
            Ok(flags) => {
                runflags = flags;
            }
            Err(err) => return Err(err),
        }

        // Set arguments
        let progname: String = env::args().next().unwrap();
        let c_progname = ffi::CString::new(progname).unwrap();
        let c_progname_ptr = c_progname.as_ptr();
        let args: Vec<ffi::CString> = runflags
            .iter()
            .map(|x| ffi::CString::new(x.clone()).unwrap())
            .collect();
        let c_progname_iter = iter::once(c_progname_ptr);
        let c_args: Vec<*const os::raw::c_char> = c_progname_iter
            .chain(args.iter().map(|x| x.as_ptr()))
            .collect();
        unsafe {
            //  Set arguments (run_options)
            options_progname = c_progname_ptr;
            options_argc = c_args.len() as u32;
            options_argv = c_args.as_ptr();
        };
        unsafe {
            run();
        }
        Ok(())
    }
}

const COMMANDS: &[&dyn Command] = &[
    &CommandAnalyze {},
    &CommandSyntax {},
    &CommandElab {},
    &CommandRun {},
    &CommandRemove {},
];

fn get_parser(args: &[String]) -> Result<(), ParseStatus> {
    for parser in COMMANDS {
        if parser.get_command().into_iter().any(|&cmd| cmd == args[0]) {
            return parser.execute(args);
        }
    }
    return Err(ParseStatus::UnknownCommand);
}

fn expand_args(args : Vec<String>) -> io::Result<Vec<String>>
{
    //  Check if there is one arg to expand
    let mut need_expand = false;
    for arg in &args {
        if arg.starts_with('@') {
            need_expand = true;
            break;
        }
    }
    if !need_expand {
        return Ok(args);
    }
    let mut res = Vec::<String>::new();
    for arg in args {
        if arg.starts_with('@') {
            use std::io::BufRead;
            let filename = &arg[1..];
            let file;
            match std::fs::File::open(filename) {
                Ok(f) => { file = f; }
                Err(e) => { eprintln!("cannot open {filename}: {e}"); return Err(e); }
            }
            let reader = std::io::BufReader::new(file);
            for line in reader.lines() {
                res.push(line?);
            }
        }
        else {
            res.push(arg);
        }
    }
    return Ok(res);
}

fn main() {
    unsafe {
        ghdl_rust_init();
    }

    let init_args : Vec<String> = env::args().collect();
    let args : Vec<String>;
    match expand_args(init_args) {
        Ok(eargs) => { args = eargs; }
        Err(_) => {
            std::process::exit(1);
        }
    }

    let progname = &args[0];

    if args.len() < 2 {
        eprintln!("missing command, try {progname} help");
        std::process::exit(1);
    }

    let command = &args[1];

    unsafe {
        set_program_name(progname.as_ptr(), progname.len() as u32);
        errorout_console_install_handler();
        options_initialize();
    };

    match get_parser(&args[1..]) {
        Ok(()) => {}
        Err(ParseStatus::UnknownCommand) => {
            eprintln!("unknown command '{command}', try {progname} help");
            std::process::exit(1);
        }
        Err(ParseStatus::OptionError) => {
            eprintln!("option error in command '{command}'\n");
            std::process::exit(1);
        }
        Err(ParseStatus::UnknownOption { msg }) => {
            eprintln!("unknown option '{msg}' in command '{command}'\n");
            std::process::exit(1);
        }
        Err(ParseStatus::NotOption) => {
            eprintln!("unexpected non-option in command\n");
            std::process::exit(1);
        }
        Err(ParseStatus::CommandExpectedError) => {},
        Err(ParseStatus::CommandError) => { std::process::exit(1)},
    }
}
