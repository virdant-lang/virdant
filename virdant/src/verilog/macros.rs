macro_rules! verilog_write {
    ($writer:expr, $fmt:literal) => {{
        verilog_write!($writer, $fmt,)
    }};
    ($writer:expr, $fmt:literal, $($arg:expr),*) => {{
        if $writer.skip_indent {
            $writer.skip_indent = false;
        } else {
            let indentation = " ".repeat(($writer.indent * 4) as usize);
            write!($writer.file, "{indentation}")?;
        }
        write!($writer.file, $fmt, $($arg)*)
    }};
}

macro_rules! verilog_writeln {
    ($writer:expr, $fmt:literal) => {{
        verilog_writeln!($writer, $fmt,)
    }};
    ($writer:expr, $fmt:literal, $($arg:expr),*) => {{
        if $writer.skip_indent {
            $writer.skip_indent = false;
        } else {
            let indentation = " ".repeat(($writer.indent * 4) as usize);
            write!($writer.file, "{indentation}")?;
        }
        writeln!($writer.file, $fmt, $($arg)*)
    }};
    ($writer:expr) => {{
        if $writer.skip_indent {
            $writer.skip_indent = false;
        } else {
            let indentation = " ".repeat(($writer.indent * 4) as usize);
            write!($writer.file, "{indentation}")?;
        }
        writeln!($writer.file)
    }};
}

pub(super) use verilog_write;
pub(super) use verilog_writeln;