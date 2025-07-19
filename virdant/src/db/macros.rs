#[macro_export]
macro_rules! define_queries {
    (
        $( input $iname:ident from $ipath:path; )*
        $( query $qname:ident from $qpath:path; )*
    ) => {
        use defile::defile;
        use paste::paste;

        #[derive(Clone, Hash, PartialEq, Eq, Debug)]
        #[allow(non_camel_case_types)]
        pub enum Query {
            $( $iname( defile! { @$ipath::Params } ),)*
            $( $qname( defile! { @$qpath::Params } ),)*
        }

        #[derive(Clone)]
        #[allow(non_camel_case_types)]
        pub enum QueryResult {
            $( $iname(defile! { @$ipath::Response } ),)*
            $( $qname(defile! { @$qpath::Response } ),)*
        }

        impl Query {
            pub(crate) fn build(self, db: &crate::db::Db) -> crate::db::CachedVal {
                match self.clone() {
                    $( Query::$iname(_params) => unreachable!(), )*
                    $(
                        Query::$qname(params)  => {
                            let mut builder = crate::db::Builder::new(db);
                            paste! {
                                let built_val = defile! { @$qpath:: [<build_ $qname>] } (&mut builder, params);
                            }
                            let val = QueryResult::$qname(built_val);
                            crate::db::CachedVal {
                                val,
                                rev: builder.db.rev,
                                deps: builder.deps.into_iter().collect(),
                            }
                        },
                    )+
                }
            }

            pub(crate) fn is_input(&self) -> bool {
                match self.clone() {
                    $( Query::$iname(_params)  => true, )*
                    $( Query::$qname(_params)  => false, )*
                }
            }
        }

        impl QueryResult {
            pub(crate) fn try_cast<T: ::std::any::Any + Clone>(&self) -> Option<T> {
                let any_val: &dyn ::std::any::Any = match self {
                    $( QueryResult::$iname(x) => x,)*
                    $( QueryResult::$qname(x) => x,)*
                };
                any_val
                    .downcast_ref::<T>()
                    .cloned()
            }

            pub(crate) fn cast<T: ::std::any::Any + Clone>(&self) -> T {
                self.try_cast()
                    .unwrap_or_else(|| {
                        panic!("Tried to cast as type {}.", std::any::type_name::<T>())
                    })
            }
        }

        impl<'d> crate::db::Builder<'d> {
            $(
                paste! {
                    pub fn [< get_ $iname >](&mut self, params: defile! { @$ipath::Params }) -> ::defile::defile! { @$ipath::Response } {
                        self.get::<defile! { @$ipath::Response }>(Query::$iname(params))
                    }
                }
            )*
            $(
                paste! {
                    pub fn [< get_ $qname >](&mut self, params: defile! { @$qpath::Params }) -> ::defile::defile! { @$qpath::Response } {
                        self.get::<defile! { @$qpath::Response }>(Query::$qname(params))
                    }
                }
            )*
        }

        impl crate::db::Db {
            $(
                paste! {
                    pub fn [< set_ $iname >](&mut self, params: defile! { @$ipath::Params }, val: defile! { @$ipath::Response }) {
                        self.clear_errors();
                        self.rev += 1;

                        let query = Query::$iname(params);
                        let val = crate::db::CachedVal {
                            val: QueryResult::$iname(val),
                            rev: self.rev,
                            deps: vec![],
                        };

                        let mut map = self.map.lock().unwrap();
                        if let Some(entry) = map.get_mut(&query) {
                            *entry = val;
                        } else {
                            map.insert(query, val);
                        }
                    }
                }

                paste! {
                    pub fn [< get_ $iname >](&self, params: defile! { @$ipath::Params }) -> ::defile::defile! { @$ipath::Response } {
                        let query = Query::$iname(params);
                        self.try_get::<defile! { @$ipath::Response }>(query).unwrap_or_default()
                    }
                }
            )*
            $(
                paste! {
                    pub fn [< get_ $qname >](&self, params: defile! { @$qpath::Params }) -> ::defile::defile! { @$qpath::Response } {
                        let query = Query::$qname(params);
                        self.get::<defile! { @$qpath::Response }>(query)
                    }
                }
            )*
        }
    };
}
