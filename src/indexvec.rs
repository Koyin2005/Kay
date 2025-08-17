use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};
#[derive(PartialEq, Eq, Clone, Hash, Default)]
///A vector where each element has an associated index
///(Where all indexes have a type).
pub struct IndexVec<I, V>(Vec<V>, PhantomData<I>);

impl<I, V> IndexVec<I, V> {
    pub const fn new() -> Self {
        Self(Vec::new(), PhantomData)
    }

    pub fn iter(&self) -> impl Iterator<Item = &V> {
        self.0.iter()
    }
}
impl<I: Idx, V> IndexVec<I, V> {
    fn next_index(&self) -> I {
        I::new_from_index(self.0.len())
    }
    pub fn push(&mut self, value: V) -> I {
        let next_index = self.next_index();
        self.0.push(value);
        next_index
    }
    pub fn get(&self, index: I) -> Option<&V> {
        self.0.get(index.into_index())
    }
    pub fn indices(&self) -> impl Iterator<Item = I> {
        (0..self.0.len()).map(I::new_from_index)
    }
    pub fn iter_enumerated(&self) -> impl Iterator<Item = (I, &V)> {
        self.indices().zip(self.0.iter())
    }
    pub fn as_slice(&self) -> &[V] {
        &self.0
    }
    pub fn into_iter_enumerated(self) -> impl Iterator<Item = (I, V)> {
        (0..self.0.len()).map(I::new_from_index).zip(self.0)
    }
    pub const fn len(&self) -> usize {
        self.0.len()
    }
}

impl<I: Idx, V: std::fmt::Debug> std::fmt::Debug for IndexVec<I, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl<'a, I: Idx, V> IntoIterator for &'a IndexVec<I, V> {
    type Item = &'a V;
    type IntoIter = std::slice::Iter<'a, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl<I: Idx, V> IntoIterator for IndexVec<I, V> {
    type Item = V;
    type IntoIter = std::vec::IntoIter<V>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<I: Idx, V> FromIterator<V> for IndexVec<I, V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Self(Vec::from_iter(iter), PhantomData)
    }
}
impl<I: Idx, V> Index<I> for IndexVec<I, V> {
    type Output = V;
    fn index(&self, index: I) -> &Self::Output {
        &self.0[index.into_index()]
    }
}
impl<I: Idx, V> IndexMut<I> for IndexVec<I, V> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.0[index.into_index()]
    }
}

///A numerical index that can be used with an index vec.
pub trait Idx {
    fn into_index(self) -> usize;

    fn new_from_index(index: usize) -> Self;
}

#[macro_export]
///Defines an id that can be used with an index vec.
macro_rules! define_id {
    (
        $(#[$($attrss:tt)*])* $v:vis struct $name:ident {}) => {

        $(#[$($attrss)*])*
        #[derive(Copy,Clone,PartialEq,Eq,PartialOrd,Ord,Hash)]
        $v struct $name(u32);

        impl $crate::indexvec::Idx for $name{
            fn into_index(self) -> usize{
                self.0 as usize
            }

            fn new_from_index(index: usize) -> Self{
                Self::new(index)
            }
        }

        impl $name{
            pub fn new(index : usize) -> Self{
                assert!(index < u32::MAX as usize, "Index too big");
                Self(index as u32)
            }
        }
    };
}
