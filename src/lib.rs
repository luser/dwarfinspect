extern crate cursive;
extern crate cursive_table_view;
#[macro_use]
extern crate failure;
extern crate fallible_iterator;
extern crate gimli;
extern crate memmap;
extern crate object;

use failure::Error;
use cursive::traits::*;
use cursive::views::{BoxView, LinearLayout, TextView};
use cursive::Cursive;
use cursive::theme::Effect;
use fallible_iterator::FallibleIterator;
use object::Object;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::env;
use std::fs::File;
use std::path::Path;
use std::thread;
use std::time::{Duration, Instant};

trait OptionError<T> {
    fn context(self, s: &'static str) -> Result<T, Error>;
}

impl<T> OptionError<T> for Option<T> {
    fn context(self, s: &'static str) -> Result<T, Error> {
        self.ok_or_else(|| failure::err_msg(s))
    }
}

fn with_file<P, F>(path: P, func: F) -> Result<(), Error>
    where F: Fn(&object::File) -> Result<(), Error>,
          P: AsRef<Path>,
{
    let path = path.as_ref();
    let f = File::open(path)?;
    let buf = unsafe { memmap::Mmap::map(&f)? };
    let obj = object::File::parse(&*buf)
        .map_err(|e| format_err!("Failed to parse file {:?}: {}", path, e))?;
    func(&obj)
}

use cursive_table_view::{TableView, TableViewItem};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum DieColumn {
    Offset,
    Tag,
    Name,
}

#[derive(Clone, Debug)]
struct Die {
    cu: usize,
    offset: usize,
    depth: usize,
    tag: gimli::DwTag,
    name: String,
}

impl TableViewItem<DieColumn> for Die {
    fn to_column(&self, column: DieColumn) -> String {
        match column {
            DieColumn::Offset => format!("{:08x}", self.offset),
            DieColumn::Tag => {
                let s = self.tag.to_string();
                let pad = " ".repeat(self.depth);
                if s.starts_with("DW_TAG_") {
                    format!("{}{}", pad, &s[7..])
                } else {
                    format!("{}{:#x}", pad, self.tag.0)
                }
            }
            DieColumn::Name => self.name.clone(),
        }
    }

    fn cmp(&self, other: &Self, column: DieColumn) -> Ordering where Self: Sized {
        match column {
            DieColumn::Offset => self.offset.cmp(&other.offset),
            DieColumn::Tag => self.tag.0.cmp(&other.tag.0),
            DieColumn::Name => self.name.cmp(&other.name),
        }
    }

}

struct FileData<'data, Endian>
    where Endian: gimli::Endianity + Send + Sync + 'static,
{
    debug_abbrev: gimli::DebugAbbrev<gimli::EndianBuf<'data, Endian>>,
    debug_info: gimli::DebugInfo<gimli::EndianBuf<'data, Endian>>,
    debug_info_len: usize,
    debug_str: gimli::DebugStr<gimli::EndianBuf<'data, Endian>>,
}

fn read_file<'data, Endian>(file: &object::File<'data>, endian: Endian) -> Result<FileData<'data, Endian>, Error>
    where Endian: gimli::Endianity + Send + Sync + 'static,
{
    let debug_abbrev_data = file.section_data_by_name(".debug_abbrev")
        .context("Failed to find .debug_abbrev section")?;
    let debug_abbrev = gimli::DebugAbbrev::new(&debug_abbrev_data, endian);
    let debug_info_data = file.section_data_by_name(".debug_info")
        .context("Failed to find .debug_info section")?;
    let debug_info_len = debug_info_data.len();
    let debug_info = gimli::DebugInfo::new(&debug_info_data, endian);
    let debug_str_data = file.section_data_by_name(".debug_str")
        .context("Failed to find .debug_str section")?;
    let debug_str = gimli::DebugStr::new(&debug_str_data, endian);

    Ok(FileData {
        debug_abbrev,
        debug_info,
        debug_info_len,
        debug_str,
    })
}

impl<'data, Endian> FileData<'data, Endian>
    where Endian: gimli::Endianity + Send + Sync + 'static,
{
    pub fn list_dies<F>(&self, mut cb: F) -> Result<(usize, Vec<Die>), Error>
        where F: FnMut(usize, usize, u32),
    {
        let mut units = self.debug_info.units().enumerate();
        let mut dies = vec![];
        let mut cu_count = 0;
        let mut die_count = 0;
        while let Some((cu_index, unit)) = units.next()? {
            cu_count += 1;
            let abbrevs = unit.abbreviations(&self.debug_abbrev)?;
            let mut entries = unit.entries(&abbrevs);
            let mut depth = 0;
            while let Some((change, entry)) = entries.next_dfs()? {
                depth += change;
                die_count += 1;
                let name = match entry.attr(gimli::DW_AT_name)?.and_then(|a| {
                    a.string_value(&self.debug_str)
                }) {
                    Some(name) => name.to_string_lossy(),
                    _ => Cow::Borrowed(""),
                };
                dies.push(Die {
                    cu: cu_index,
                    offset: entry.offset().to_debug_info_offset(&unit).0,
                    depth: depth as usize,
                    tag: entry.tag(),
                    name: name.into_owned(),
                });
            }
            let offset = unit.offset().0 + unit.length_including_self();
            let percent = ((offset as f64) / (self.debug_info_len as f64) * 100.0) as u32;
            cb(cu_count, die_count, percent);
        }
        Ok((cu_count, dies))
    }
}

/// Format `duration` as seconds with a fractional component.
fn fmt_duration_as_secs(duration: &Duration) -> String
{
    format!("{}.{:03} s", duration.as_secs(), duration.subsec_nanos() / 1000_000)
}

pub fn main() -> Result<(), Error> {
    let filename = env::args_os().nth(1).ok_or_else(|| format_err!("Usage: dwarfinspect <file>"))?;
    let mut siv = Cursive::default();
    siv.set_fps(10);
    let sink = siv.cb_sink().clone();
    // Load the DWARF on a background thread just so we can display the initial UI while loading.
    let child = thread::spawn(move || {
        with_file(&filename, |f| {
            let endian = if f.is_little_endian() {
                gimli::RunTimeEndian::Little
            } else {
                gimli::RunTimeEndian::Big
            };
            let start = Instant::now();
            let data = read_file(f, endian)?;
            let (cu_count, dies) = data.list_dies(|cu_count, die_count, percent| {
                drop(sink.send(Box::new(move |s: &mut Cursive| {
                    s.call_on_id("status", |view: &mut TextView| {
                        view.set_content(format!("Loading: {:3}%: {} CUs, {} DIEs", percent,
                                                 cu_count, die_count));
                    });
                })));
            })?;
            let elapsed = start.elapsed();
            let die_count = dies.len();
            drop(sink.send(Box::new(move |s: &mut Cursive| {
                s.call_on_id("table", |view: &mut TableView<Die, DieColumn>| {
                    view.set_items(dies);
                });
                s.call_on_id("status", |view: &mut TextView| {
                    view.set_content(format!("Loaded {} CUs, {} DIEs in {}",
                                             cu_count,
                                             die_count,
                                             fmt_duration_as_secs(&elapsed)));
                });
            })));
            Ok(())
        })
    });

    siv.add_global_callback('q', |s| s.quit());
    siv.add_fullscreen_layer(BoxView::with_full_screen(
        LinearLayout::vertical()
            .child(
                TableView::<Die, DieColumn>::new()
                // Per the offset formatting
                    .column(DieColumn::Offset, "Offset", |c| c.width(8))
                // Determined by running this in a gimli checkout:
                // grep DW_TAG src/constants.rs  | sed -e 's/.*DW_TAG_//' -e 's/ = .*//' | wc -L
                // plus a few extra spaces for indents
                    .column(DieColumn::Tag, "Tag", |c| c.width(32))
                    .column(DieColumn::Name, "Name", |c| c)
                    .items(vec![])
                    .with_id("table")
                    .full_width()
                    .full_height())
            .child(
                TextView::new("Loading...")
                    .effect(Effect::Reverse)
                    .with_id("status")
                    .full_width()
                    .max_height(1)
            )));
    siv.run();
    drop(child.join());
    Ok(())
}
