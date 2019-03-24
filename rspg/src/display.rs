use std::fmt;

pub trait DisplayWith<With: ?Sized> {
    fn fmt(&self, f: &mut std::fmt::Formatter, with: &With) -> std::fmt::Result;
    fn display_with<'t, 'w>(&'t self, with: &'w With) -> DisplayWithMixer<'t, 'w, Self, With> {
        DisplayWithMixer(self, with)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DisplayWithMixer<'t, 'w, T, With>(&'t T, &'w With)
where
    T: DisplayWith<With> + ?Sized,
    With: ?Sized;

impl<'t, 'w, T, With> fmt::Display for DisplayWithMixer<'t, 'w, T, With>
where
    T: DisplayWith<With>,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f, self.1)
    }
}
