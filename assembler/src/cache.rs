use std::{collections::HashMap, fmt::Display, hash::Hash};

use ariadne::Source;

pub trait CacheStr<Id> {
    type Error;
    fn get_str<'a>(&'a mut self, id: &Id) -> Result<&'a str, Self::Error>;
}

type Cache<Id> = HashMap<Id, (String, Source)>;

pub struct FnCache<Id, F> {
    cache: Cache<Id>,
    get: F,
}

impl<Id, F> FnCache<Id, F> {
    pub fn new(get: F) -> Self {
        Self {
            cache: Default::default(),
            get,
        }
    }
}

impl<'a, Id: Hash + Eq + Clone, F> FnCache<Id, F> {
    fn get<S: Into<String>, E>(&mut self, id: &'a Id) -> Result<&(String, Source), E>
    where
        F: Fn(&'a Id) -> Result<S, E> + 'a,
    {
        Ok(self.cache.entry(id.clone()).or_insert(
            (self.get)(id)
                .map(Into::into)
                .map(|x| (x.clone(), Source::from(x)))?,
        ))
    }
}

// impl<S: Into<String>, E, F: Fn() -> Result<S, E>> FnCache<OnlyOne, F> {
// 	fn get_one(&mut self) -> Result<&(String, Source), E> {
// 		Ok(self.cache.entry(OnlyOne).or_insert((self.get)().map(Into::into).map(|x| (x.clone(), Source::from(x)))?))
// 	}
// }

impl<Id: Hash + Eq + Clone, S: Into<String>, E, F: Fn(&Id) -> Result<S, E>> CacheStr<Id>
    for FnCache<Id, F>
{
    type Error = E;

    fn get_str<'a>(&'a mut self, id: &Id) -> Result<&'a str, Self::Error> {
        self.get(id).map(|(s, _)| s.as_str())
    }
}

impl<
        Id: Hash + Eq + Clone + Display,
        S: Into<String>,
        E: std::fmt::Debug + 'static,
        F: Fn(&Id) -> Result<S, E>,
    > ariadne::Cache<Id> for FnCache<Id, F>
{
    fn fetch(&mut self, id: &Id) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        self.get(id)
            .map(|(_, x)| x)
            .map_err::<Box<dyn std::fmt::Debug>, _>(|x| Box::new(x))
    }

    fn display<'a>(&self, id: &'a Id) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(id))
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct OnlyOne;

impl Display for OnlyOne {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<unknown>")
    }
}
