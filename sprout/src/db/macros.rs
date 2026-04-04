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
        pub(crate) enum Query {
            $(
                $query($($typ),*),
            )*
        }

        #[derive(Clone, Debug)]
        pub(crate) enum QueryResult {
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
            #[track_caller]
            pub fn $getter_name(&self, $($arg : $argtyp),*) -> $rettyp {
                let location = std::panic::Location::caller();
                let query = Query::$query($($arg),*);
                let cached_val = self.get_or_build(query, *location);
                cast!(cached_val.val, $query)
            }
        }
        #[allow(dead_code)]
        impl<'d> Builder<'d> {
            #[track_caller]
            pub(crate) fn $getter_name(&mut self, $($arg : $argtyp),*) -> $rettyp {
                let location = std::panic::Location::caller();
                let query = Query::$query($($arg),*);
                cast!(self.get(query, *location), $query)
            }
        }
    };
}

