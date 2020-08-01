#[macro_export]
macro_rules! grammar {
    (start $s:ident; $($tail:tt)+) => {
        {
            use $crate::grammar::GrammarBuilder;
            let builder = GrammarBuilder::<&'static str, _>::new()
                .start(stringify!($s));
            let builder = grammar!(with builder builder | $($tail)+);
            builder.build()
        }
    };
    (with builder $builder:ident | rule $left:ident -> $($tail:tt)+) => {
        {
            let builder = $builder.push_rule_left(stringify!($left));
            grammar!(with builder builder in rule | $($tail)+)
        }
    };
    (with builder $builder:ident | ) => {
        {
            $builder
        }
    };
    (with builder $builder:ident in rule | epsilon ; $($tail:tt)*) => {
        {
            grammar! { with builder $builder | $($tail)* }
        }
    };
    (with builder $builder:ident in rule | $n:ident, $($tail:tt)+) => {
        {
            let builder = $builder.push_rule_right_nonterminal(stringify!($n));
            grammar! ( with builder builder in rule | $($tail)+ )
        }
    };
    (with builder $builder:ident in rule | $t:expr, $($tail:tt)+) => {
        {
            let builder = $builder.push_rule_right_terminal($t);
            grammar! { with builder builder in rule | $($tail)+ }
        }
    };
    (with builder $builder:ident in rule | $n:ident; $($tail:tt)*) => {
        {
            let builder = $builder.push_rule_right_nonterminal(stringify!($n));
            grammar! { with builder builder | $($tail)* }
        }
    };
    (with builder $builder:ident in rule | $t:expr; $($tail:tt)*) => {
        {
            let builder = $builder.push_rule_right_terminal($t);
            grammar! { with builder builder | $($tail)* }
        }
    };
}
