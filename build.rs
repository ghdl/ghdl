// build.rs

use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufWriter, Write};
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
// use std::path::Path;

fn c_compile(out_dir: &str, optflags: &[&str], src: &str, cflags: &[&str]) -> Result<(), ()> {
    let file = Path::new(src);
    let basename = file.file_stem().unwrap();
    let mut objname = PathBuf::from(out_dir);
    objname.push(&basename);
    objname.set_extension("o");
    let obj = objname.as_path().to_str().unwrap();

    let status = Command::new("gcc")
        .arg("-c")
        .args(optflags)
        .arg(src)
        .args(cflags)
        .args(&["-o", obj])
        .status()
        .unwrap();
    if !status.success() {
        return Err(());
    }
    println!("cargo:rerun-if-changed={}", src);
    println!("cargo:rustc-link-arg={}", obj);
    return Ok(());
}

fn ada_compile(
    top: &str,
    mode: &str,
    out_dir: &str,
    optflags: &[&str],
    src_dirs: &[&str],
    bargs: &[&str],
) -> Result<(), ()> {
    let cmd = "gnatmake";
    let mut args = Vec::<String>::new();
    args.push(String::from(mode));
    args.push(String::from("-D"));
    args.push(String::from(out_dir));
    for &opt in optflags {
        args.push(String::from(opt));
    }

    if src_dirs.len() != 0 {
        args.push(String::from("-I-"));
        for &dir in src_dirs {
            let mut opt = String::from("-aI");
            opt += dir;
            args.push(opt);
        }
    }
    args.push(String::from(top));
    for &arg in bargs {
        args.push(String::from(arg));
    }
    if true {
        eprint!("{cmd}");
        for arg in &args {
            eprint!(" {}", arg);
        }
        eprintln!();
    }
    let status = Command::new(cmd).args(&args).status().unwrap();
    if !status.success() {
        return Err(());
    }
    Ok(())
}

fn run() -> Result<(), ()> {
    let out_dir = env::var("OUT_DIR").unwrap();
    let bind_file = format!("{}/bind_rust.adb", out_dir);

    let optflags = ["-fPIC", "-g"];

    let ada_src_dirs = [
        ".",
        "src",
        "src/vhdl",
        "src/psl",
        "src/verilog",
        "src/synth",
        "src/grt",
        "src/ortho",
        "src/ortho/mcode",
        "src/vhdl/translate",
        "src/simul",
        "src/ghdldrv",
    ];
    let ada_top = "ghdl_rust";
    let bargs = ["-bargs", "-n", "-o", &bind_file, "-Lghdl_rust_"];

    //  Call gnatmake to compile
    ada_compile(&ada_top, "-c", &out_dir, &optflags, &ada_src_dirs, &[])?;

    //  Call gnatmake to create the binder file
    ada_compile(&ada_top, "-b", &out_dir, &optflags, &ada_src_dirs, &bargs)?;

    //  Fix the binder file...
    {
        let bind_content: String = std::fs::read_to_string(&bind_file).unwrap();
        let mut f = BufWriter::new(File::create(&bind_file).unwrap());
        let mut rewrite_source = true;
        for l in bind_content.lines() {
            if l.starts_with("pragma Source_File_Name") {
                const SPEC_PRAGMA: &str =
                    "pragma Source_File_Name (ghdl_rust_main, Spec_File_Name => \"bind_rust.ads\");\n";
                const BODY_PRAGMA: &str =
                    "pragma Source_File_Name (ghdl_rust_main, Body_File_Name => \"bind_rust.adb\");\n";
                if rewrite_source {
                    f.write_all(SPEC_PRAGMA.as_bytes()).unwrap();
                    f.write_all(BODY_PRAGMA.as_bytes()).unwrap();
                    rewrite_source = false;
                }
            } else {
                f.write_all(l.as_bytes()).unwrap();
                f.write_all(&[b'\n']).unwrap();
            }
        }
    }

    //  Compile the binder file
    ada_compile(&bind_file, "-c", &out_dir, &optflags, &[], &[])?;
    //    Command::new(cmd)
    //        .args(["-c", "-gnatA", "-gnatWb", "-gnatiw", "-D", &out_dir, "-fPIC", &bind_file])
    //        .status()
    //        .unwrap();

    //  Add the binder file to the linker
    //  It most appear before the '-lXXX' flags
    println!("cargo:rustc-link-arg={}/bind_rust.o", &out_dir);

    //  Add flags from the binder files
    let file = File::open(bind_file).unwrap();
    let mut in_objs = false;
    for l in io::BufReader::new(file).lines() {
        let ls = l.unwrap().to_string();
        if !in_objs && ls.starts_with("--  BEGIN Object") {
            in_objs = true;
        } else if in_objs && ls.starts_with("--  END Object") {
            in_objs = false;
        } else if in_objs {
            let (comment, link_arg) = ls.split_at(8);
            assert!(comment == "   --   ");
            if link_arg.starts_with("/") || link_arg.starts_with("-l") {
                println!("cargo:rustc-link-arg={}", link_arg);
            } else {
                eprintln!("discard: {}", link_arg);
            }
        }
    }

    //    println!("cargo:rustc-link-search=native={}", out_dir);
    //    println!("cargo:rustc-link-lib=static=hello");
    println!("cargo:rerun-if-changed=build.rs");

    //  TODO:
    //  automatically generate the list of dependency.
    //  Search corresponding source files (.ads and .adb) from object files ?
    println!("cargo:rerun-if-changed=src/ghdldrv/ghdl_rust.ads");

    c_compile(&out_dir, &optflags, "src/grt/grt-cstdio.c", &[])?;

    let cflags = &["-Isrc/grt/fst"];
    c_compile(&out_dir, &optflags, "src/grt/fst/fstapi.c", cflags)?;
    c_compile(&out_dir, &optflags, "src/grt/fst/lz4.c", cflags)?;
    c_compile(&out_dir, &optflags, "src/grt/fst/fastlz.c", cflags)?;

    c_compile(&out_dir, &optflags, "src/grt/grt-no_sundials_c.c", &[])?;
    c_compile(&out_dir, &optflags, "src/grt/grt-cdynload.c", &[])?;
    c_compile(&out_dir, &optflags, "src/grt/config/jumps.c", &[])?;
    c_compile(&out_dir, &optflags, "src/grt/config/times.c", &[])?;
    c_compile(&out_dir, &optflags, "src/grt/grt-cvpi.c", &[])?;
    c_compile(&out_dir, &optflags, "src/grt/grt-cvhpi.c", &[])?;
    c_compile(&out_dir, &optflags, "src/ortho/mcode/memsegs_c.c", &[])?;

    Ok(())
}

fn main() {
    match run() {
        Ok(_) => {}
        Err(_) => {
            std::process::exit(1);
        }
    }
}
