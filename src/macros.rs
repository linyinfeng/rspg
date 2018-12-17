#[macro_export]
macro_rules! grammar {
    ($($t:tt)+) => {
        grammar_impl!{$($t)+}
    };
}

#[macro_export]
macro_rules! grammar_impl {
    (start $s:ident; $($tail:tt)+) => {
        {
            use $crate::grammar::GrammarBuilder;
            let builder = GrammarBuilder::new()
                .start(stringify!($s));
            let builder = grammar_impl!(with builder builder | $($tail)+);
            builder.build().unwrap()
        }
    };
    (with builder $builder:ident | rule $left:ident -> $($tail:tt)+) => {
        {
            let builder = $builder.push_rule(stringify!($left));
            grammar_impl!(with builder builder in rule | $($tail)+)
        }
    };
    (with builder $builder:ident | ) => {
        {
            $builder
        }
    };
    (with builder $builder:ident in rule | ε ; $($tail:tt)*) => {
        {
            grammar_impl! { with builder $builder | $($tail)* }
        }
    };
    (with builder $builder:ident in rule | $n:ident, $($tail:tt)+) => {
        {
            use $crate::grammar::Symbol;
            let builder = $builder.push_right(Symbol::Nonterminal(stringify!($n)));
            grammar_impl! ( with builder builder in rule | $($tail)+ )
        }
    };
    (with builder $builder:ident in rule | $t:expr, $($tail:tt)+) => {
        {
            use $crate::grammar::Symbol;
            let builder = $builder.push_right(Symbol::Terminal($t));
            grammar_impl! { with builder builder in rule | $($tail)+ }
        }
    };
    (with builder $builder:ident in rule | $n:ident; $($tail:tt)*) => {
        {
            use $crate::grammar::Symbol;
            let builder = $builder.push_right(Symbol::Nonterminal(stringify!($n)));
            grammar_impl! { with builder builder | $($tail)* }
        }
    };
    (with builder $builder:ident in rule | $t:expr; $($tail:tt)*) => {
        {
            use $crate::grammar::Symbol;
            let builder = $builder.push_right(Symbol::Terminal($t));
            grammar_impl! { with builder builder | $($tail)* }
        }
    };
}
