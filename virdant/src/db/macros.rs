macro_rules! cast {
    ($query_result:expr, $query:ident) => {{
        if let QueryResult::$query(value) = $query_result {
            value
        } else {
            panic!("Expected QueryResult::{}", stringify!($query))
        }
    }};
}

macro_rules! queries {
    ($(
            $query:ident($($arg:ident : $typ:path),*) -> $ret:path;)*
    ) => {
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        enum Query {
            $(
                $query($($typ),*),
            )*
        }

        #[derive(Clone, Debug)]
        enum QueryResult {
            $(
                $query($ret),
            )*
        }
    }
}

macro_rules! dispatch_build {
    ($self_:expr, $db:expr; $( $buildfn:path : $query:ident ($($args:tt),*); )*) => {
        match $self_ {
            $(
                Query::$query($( $args, )*) => {
                    let mut builder = Builder::new($db);
                    let built_val = $buildfn(&mut builder, $( $args.clone(), )*);
                    let val = QueryResult::$query(built_val).into();
                    CachedVal {
                        val,
                        rev: builder.db.rev,
                        deps: builder.deps.into_iter().collect(),
                    }
                }
            )*
            _ => unreachable!("Tried to dispatch build"),
        }
    }
}

macro_rules! db_getter {
    ($getter_name:ident : $query:ident($($arg:ident : $argtyp:path),*) -> $rettyp:path) => {
        impl Db {
            pub fn $getter_name(&self, $($arg : $argtyp),*) -> $rettyp {
                let query = Query::$query($($arg),*);
                cast!(self.get(query), $query)
            }
        }
        impl<'d> Builder<'d> {
            pub(crate) fn $getter_name(&mut self, $($arg : $argtyp),*) -> $rettyp {
                let query = Query::$query($($arg),*);
                cast!(self.get(query), $query)
            }
        }
    };
}

