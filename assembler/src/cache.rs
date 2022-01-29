use std::{
    collections::HashMap,
    fmt::Display,
    hash::Hash,
    path::{Path, PathBuf},
};

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

pub struct StrCache {
    source: Source,
    s: &'static str
}

impl StrCache {
    pub fn new(s: &'static str) -> Self {
        Self {
            source: s.into(),
            s
        }
    }
}

impl Clone for StrCache {
    fn clone(&self) -> Self {
        Self { source: self.s.into(), s: self.s }
    }
}

impl CacheStr<OnlyOne> for StrCache{
    type Error = ();

    fn get_str<'a>(&'a mut self, _: &OnlyOne) -> Result<&'a str, Self::Error> {
        Ok(self.s)
    }
}

impl ariadne::Cache<OnlyOne> for StrCache {
    fn fetch(&mut self, _: &OnlyOne) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        Ok(&self.source)
    }

    fn display<'a>(&self, id: &'a OnlyOne) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(id))
    }
}

#[derive(Default)]
pub struct FsCache {
    cache: Cache<PathBuf>,
    valid: Vec<PathBuf>,
}

impl FsCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add<P: AsRef<Path>>(&mut self, p: &P) {
        self.valid.push(p.as_ref().to_path_buf())
    }

    fn get<P: AsRef<Path>>(&mut self, p: &P) -> Result<&(String, Source), std::io::Error> {
        if self.valid.iter().any(|x| x == p.as_ref()) {
            Ok(self.cache.entry(p.as_ref().to_path_buf()).or_insert(
                std::fs::read_to_string(p)
                    .map(Into::into)
                    .map(|x: String| (x.clone(), Source::from(x)))?,
            ))
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("{} is not a valid path", p.as_ref().display()),
            ))
        }
    }
}

impl<P: AsRef<Path>> CacheStr<P> for FsCache {
    type Error = std::io::Error;

    fn get_str<'a>(&'a mut self, id: &P) -> Result<&'a str, Self::Error> {
        Ok(self.get(id)?.0.as_str())
    }
}

impl<P: AsRef<Path>> ariadne::Cache<P> for FsCache {
    fn fetch(&mut self, id: &P) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        Ok(&self
            .get(id)
            .map_err(|x| -> Box<dyn std::fmt::Debug> { Box::new(x) })?
            .1)
    }

    fn display<'a>(&self, id: &'a P) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(id.as_ref().display()))
    }
}
