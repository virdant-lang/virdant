use virdant::Virdant;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let top_path: String = args[1].clone().into();
    let mut virdant = Virdant::new(&top_path);

    match virdant.check() {
        Err(errors) => {
            eprintln!("{virdant:?}");
            eprintln!("ERRORS:");
            for error in errors.into_iter() {
                eprintln!("    {error:?}");
            }
            std::process::exit(1);
        },
        Ok(_) => println!("{virdant:?}"),
    }
}
