use diagnostic::Diagnostic;
use parser::parse;
use span::{Pos, Source, SourcePath, Span};
use std::sync::Arc;

fn main() {
    let source = r#"
pub fn test1(a cptr str, b isize) f64 {
    for {
        let a = 1;
        let b = 2;
        let c = 3;
    }

    for a != 10 {
        a += 1;
    }

    for i in 0..10000 {
        if i % 2 == 0 {
            continue;
        }

        if i % 3 == 0 {
            break;
        }

        if i % 5 == 0 {
            continue;
        }

        if i % 7 == 0 {
            break;
        }

        if i % 11 == 0 {
            continue;
        }

        println("{}", i);
    }

    let test isize;
    let test = value;
    let test isize = value;
    (a.b.c.fdasfdsfdsa.f34242423423423[redsafasdfas >> 123 as f64] as f64).dsaf;
    a..10;
    abtest[a..=10 + 215 / 1231 >> 23];
    break; cotinue;
    +++++a;
    {
        return 10;
    }
    123;
    ta;
    ta[321];
    call(a, b, expr[12241]);
    (a as f64 as f64 as f64)(10, 342);
    !~~~!~!~!~~!~!!!~!+--+-++----++((aaaaa))(1, 2, 3, 5);
    a | b as isize & 0x1000isize ^ 0x1110isize;
    0x1000isize ^ 0x1110isize;
    1000f64;
    a == b[0012];
    b != c;
    123 + 10;
    a = a + b / 10 % call(a, b, c[123 + 10 % 1231isize]) && a == 10isize;

    fn inner_fn(a isize, b isize) isize {
        if a == b {
            return 10;
        } else if a != b {
            println("a and b is not eq! a:{}, b:{}", a, b);
        } else {
            println("hello");
        }
        return a + b;
    }

    inner_fn(1, 2);
}

fn test2(a str) cptr str {
    struct Test {
        pub name cptr str,
        pub age usize,
        private_field isize,
        inner_struct struct {
            pub name cptr str,
            pub age usize,
            private_field isize,
            inner_struct struct {
                pub name cptr str,
                pub age usize,
                private_field isize,
            },
        },
    }

    struct Test {
        pub name cptr str,
        pub age usize,
        private_field isize,
        inner_struct struct {
            pub name cptr str,
            pub age usize,
            private_field isize,
            inner_struct struct {
                pub name cptr str,
                pub age usize,
                private_field isize,
            },
        },
    }

    return Test {
        name: "hello",
        age: 10,
        private_field: 10,
        inner_struct: {
            name: "hello",
            age: 10,
            private_field: 10,
            inner_struct: {
                name: "hello",
                age: 10,
                private_field: 10,
            },
        },
    };

    return "hello";
}

pub struct Test {
    pub name cptr str,
    pub age usize,
    private_field isize,
    inner_struct struct {
        pub name cptr str,
        pub age usize,
        private_field isize,
        inner_struct struct {
            pub name cptr str,
            pub age usize,
            private_field isize,
        },
    },
}

struct Test {
    pub name cptr str,
    pub age usize,
    private_field isize,
    inner_struct struct {
        pub name cptr str,
        pub age usize,
        private_field isize,
        inner_struct struct {
            pub name cptr str,
            pub age usize,
            private_field isize,
        },
    },
}
    "#;
    let source = Arc::new(Source::new(
        Span::new(Pos::new(0), Pos::new(source.len() as _)),
        "main.tt".to_owned(),
        SourcePath::Virtual("main.tt".to_owned()),
        source.to_owned(),
    ));
    let program = match parse(source.clone()) {
        Ok(program) => program,
        Err(err) => {
            let line_col = source.find_line_col(err.1.low());
            panic!(
                "{:?} [where: {:?}] [source: {}]",
                err.0,
                line_col,
                source.slice_line(line_col.line())
            );
        }
    };

    println!("{:#?}", program);

    // for token in iter {
    //     println!("{:?}", token);
    // }

    for diagnostic in Diagnostic::diagnostics().iter() {
        println!("{:?}", diagnostic);
    }
}
