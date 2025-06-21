use core::fmt;

/// Emulate a `C`-style `for` loop.
///
/// This `for` loop in `C`, for example:
///
/// ```c
/// for (int i = 1; i <= 5; i++) {
///     printf("%d\n", i);
/// }
/// ```
///
/// May be written as follows:
///
/// ```
/// # use seasick::cfor;
/// # let mut v = vec![];
/// for i in cfor(1, |i| *i <= 5, |i| *i +=1 ) {
///     println!("{}", i);
/// #   v.push(i);
/// }
/// # assert_eq!(v, [1, 2, 3, 4, 5]);
/// ```
///
/// This allows you to operate on iterators with `init`/`done`/`next` operations
/// typical of `C` APIs.
pub fn cfor<T, CondF, NextF>(init: T, cond: CondF, next: NextF) -> CFor<T, CondF, NextF>
where
    T: Clone,
    CondF: FnMut(&T) -> bool,
    NextF: FnMut(&mut T),
{
    CFor {
        current: init,
        cond,
        next,
    }
}

/// [`Iterator`] returned by [`cfor`].
#[derive(Clone)]
pub struct CFor<T, CondF = fn(T) -> bool, NextF = fn(T) -> T> {
    /// The current state of the iterated variable.
    pub current: T,
    cond: CondF,
    next: NextF,
}

impl<T: fmt::Debug, CondF, NextF> fmt::Debug for CFor<T, CondF, NextF> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CFor")
            .field("current", &self.current)
            .finish_non_exhaustive()
    }
}

impl<T, CondF, NextF> Iterator for CFor<T, CondF, NextF>
where
    T: Clone,
    CondF: FnMut(&T) -> bool,
    NextF: FnMut(&mut T),
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let Self {
            current,
            cond,
            next,
        } = self;
        match cond(current) {
            true => {
                let yieldme = current.clone();
                next(current);
                Some(yieldme)
            }
            false => None,
        }
    }
}
