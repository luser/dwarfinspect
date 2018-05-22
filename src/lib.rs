extern crate cursive;
extern crate cursive_table_view;
#[macro_use]
extern crate failure;
extern crate fallible_iterator;
extern crate gimli;
extern crate memmap;
extern crate object;

use cursive::traits::*;
use cursive::views::{BoxView, Dialog, LinearLayout, TextView};
use cursive::{CbFunc, Cursive};
use cursive::theme::Effect;
use failure::Error;
use fallible_iterator::FallibleIterator;
use object::Object;
use std::boxed::Box;
use std::cmp::Ordering;
use std::env;
use std::fmt::Write;
use std::fs::File;
use std::path::Path;
use std::sync::mpsc::{self, Receiver, Sender};
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

trait Reader: gimli::Reader<Offset = usize> {}

impl<'input, Endian> Reader for gimli::EndianBuf<'input, Endian>
where
    Endian: gimli::Endianity + Send + Sync,
{}

fn with_file<P, F>(path: P, func: F) -> Result<(), Error>
    where F: FnOnce(&object::File) -> Result<(), Error>,
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

struct Unit<R: Reader> {
    endian: R::Endian,
    format: gimli::Format,
    address_size: u8,
    version: u16,
    base_address: u64,
    line_program: Option<gimli::IncompleteLineNumberProgram<R>>,
    comp_dir: Option<R>,
    comp_name: Option<R>,
}

struct FileData<R: Reader> {
    debug_abbrev: gimli::DebugAbbrev<R>,
    debug_info: gimli::DebugInfo<R>,
    debug_info_len: usize,
    debug_str: gimli::DebugStr<R>,
    debug_line: gimli::DebugLine<R>,
    endian: R::Endian,
    units: Vec<(gimli::CompilationUnitHeader<R, R::Offset>, Unit<R>)>,
}

fn read_file<'data, Endian>(file: &object::File<'data>, endian: Endian) -> Result<FileData<gimli::EndianBuf<'data, Endian>>, Error>
    where Endian: gimli::Endianity + Send + Sync,
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
    let debug_line_data = file.section_data_by_name(".debug_line")
        .context("Failed to find .debug_line section")?;
    let debug_line = gimli::DebugLine::new(&debug_line_data, endian);

    Ok(FileData {
        debug_abbrev,
        debug_info,
        debug_info_len,
        debug_str,
        debug_line,
        endian,
        units: vec![],
    })
}

fn dump_file_index<R: Reader, W: Write>(w: &mut W, file: u64, unit: &Unit<R>) -> Result<(), Error> {
    if file == 0 {
        return Ok(());
    }
    let header = match unit.line_program {
        Some(ref program) => program.header(),
        None => return Ok(()),
    };
    let file = match header.file(file) {
        Some(header) => header,
        None => {
            writeln!(w, "Unable to get header for file {}", file)?;
            return Ok(());
        }
    };
    write!(w, " ")?;
    if let Some(directory) = file.directory(header) {
        let directory = directory.to_string_lossy()?;
        if !directory.starts_with('/') {
            if let Some(ref comp_dir) = unit.comp_dir {
                write!(w, "{}/", comp_dir.to_string_lossy()?)?;
            }
        }
        write!(w, "{}/", directory)?;
    }
    write!(w, "{}", file.path_name().to_string_lossy()?)?;
    Ok(())
}

fn dump_attr_value<R, W>(
    w: &mut W,
    attr: &gimli::Attribute<R>,
    unit: &Unit<R>,
    debug_str: &gimli::DebugStr<R>,
    //loclists: &gimli::LocationLists<R>,
    //rnglists: &gimli::RangeLists<R>,
) -> Result<(), Error>
    where R: Reader,
          W: Write,
{
    let value = attr.value();
    match value {
        gimli::AttributeValue::Addr(address) => {
            writeln!(w, "0x{:08x}", address)?;
        }
        gimli::AttributeValue::Block(data) => {
            for byte in data.to_slice()?.iter() {
                write!(w, "{:02x}", byte)?;
            }
            writeln!(w)?;
        }
        gimli::AttributeValue::Data1(_)
        | gimli::AttributeValue::Data2(_)
        | gimli::AttributeValue::Data4(_)
        | gimli::AttributeValue::Data8(_) => {
            if let (Some(udata), Some(sdata)) = (attr.udata_value(), attr.sdata_value()) {
                if sdata >= 0 {
                    writeln!(w, "{}", udata)?;
                } else {
                    writeln!(w, "{} ({})", udata, sdata)?;
                }
            } else {
                writeln!(w, "{:?}", value)?;
            }
        }
        gimli::AttributeValue::Sdata(data) => {
            match attr.name() {
                gimli::DW_AT_data_member_location => {
                    writeln!(w, "{}", data)?;
                }
                _ => if data >= 0 {
                    writeln!(w, "0x{:08x}", data)?;
                } else {
                    writeln!(w, "0x{:08x} ({})", data, data)?;
                },
            };
        }
        gimli::AttributeValue::Udata(data) => {
            match attr.name() {
                gimli::DW_AT_high_pc => {
                    writeln!(w, "<offset-from-lowpc>{}", data)?;
                }
                gimli::DW_AT_data_member_location => {
                    if let Some(sdata) = attr.sdata_value() {
                        // This is a DW_FORM_data* value.
                        // libdwarf-dwarfdump displays this as signed too.
                        if sdata >= 0 {
                            writeln!(w, "{}", data)?;
                        } else {
                            writeln!(w, "{} ({})", data, sdata)?;
                        }
                    } else {
                        writeln!(w, "{}", data)?;
                    }
                }
                gimli::DW_AT_lower_bound | gimli::DW_AT_upper_bound => {
                    writeln!(w, "{}", data)?;
                }
                _ => {
                    writeln!(w, "0x{:08x}", data)?;
                }
            };
        }
        gimli::AttributeValue::Exprloc(ref data) => {
            if let gimli::AttributeValue::Exprloc(_) = attr.raw_value() {
                write!(w, "len 0x{:04x}: ", data.0.len())?;
                for byte in data.0.to_slice()?.iter() {
                    write!(w, "{:02x}", byte)?;
                }
                write!(w, ": ")?;
            }
            //dump_exprloc(w, data, unit)?;
            writeln!(w)?;
        }
        gimli::AttributeValue::Flag(true) => {
            // We don't record what the value was, so assume 1.
            writeln!(w, "yes(1)")?;
        }
        gimli::AttributeValue::Flag(false) => {
            writeln!(w, "no")?;
        }
        gimli::AttributeValue::SecOffset(offset) => {
            writeln!(w, "0x{:08x}", offset)?;
        }
        gimli::AttributeValue::UnitRef(gimli::UnitOffset(offset)) => {
            writeln!(w, "<0x{:08x}>", offset)?;
        }
        gimli::AttributeValue::DebugInfoRef(gimli::DebugInfoOffset(offset)) => {
            writeln!(w, "<GOFF=0x{:08x}>", offset)?;
        }
        /*
        gimli::AttributeValue::DebugInfoRefSup(gimli::DebugInfoOffset(offset)) => {
            writeln!(w, "<SUP_GOFF=0x{:08x}>", offset)?;
        }
         */
        gimli::AttributeValue::DebugLineRef(gimli::DebugLineOffset(offset)) => {
            writeln!(w, "0x{:08x}", offset)?;
        }
        gimli::AttributeValue::DebugLocRef(_) => {
            writeln!(w, "")?;
        }
        /*
        gimli::AttributeValue::LocationListsRef(offset) => {
            dump_loc_list(w, loclists, offset, unit)?;
        }
         */
        gimli::AttributeValue::DebugMacinfoRef(gimli::DebugMacinfoOffset(offset)) => {
            writeln!(w, "{}", offset)?;
        }
        gimli::AttributeValue::DebugRangesRef(_) => {
            writeln!(w, "")?;
        }
        /*
        gimli::AttributeValue::RangeListsRef(offset) => {
            writeln!(w, "0x{:08x}", offset.0)?;
            dump_range_list(w, rnglists, offset, unit)?;
        }
         */
        gimli::AttributeValue::DebugTypesRef(_signature) => {
            //dump_type_signature(w, signature, unit.endian)?;
            writeln!(w, " <type signature>")?;
        }
        gimli::AttributeValue::DebugStrRef(offset) => if let Ok(s) = debug_str.get_str(offset) {
            writeln!(w, "{}", s.to_string_lossy()?)?;
        } else {
            writeln!(w, "<GOFF=0x{:08x}>", offset.0)?;
        },
        /*
        gimli::AttributeValue::DebugStrRefSup(offset) => {
            writeln!(w, "<SUP_GOFF=0x{:08x}>", offset.0)?;
        }
         */
        gimli::AttributeValue::String(s) => {
            writeln!(w, "{}", s.to_string_lossy()?)?;
        }
        gimli::AttributeValue::Encoding(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::DecimalSign(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::Endianity(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::Accessibility(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::Visibility(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::Virtuality(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::Language(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::AddressClass(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::IdentifierCase(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::CallingConvention(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::Inline(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::Ordering(value) => {
            writeln!(w, "{}", value)?;
        }
        gimli::AttributeValue::FileIndex(value) => {
            write!(w, "0x{:08x}", value)?;
            dump_file_index(w, value, unit)?;
            writeln!(w)?;
        }
    }

    Ok(())
}

impl<R> FileData<R>
    where R: Reader,
{
    pub fn list_dies<F>(&mut self, mut cb: F) -> Result<(usize, Vec<Die>), Error>
        where F: FnMut(usize, usize, u32),
    {
        let mut units = self.debug_info.units().enumerate();
        let mut dies = vec![];
        while let Some((cu_index, hdr)) = units.next()? {
            let mut unit = Unit {
                endian: self.endian,
                format: hdr.format(),
                address_size: hdr.address_size(),
                version: hdr.version(),
                base_address: 0,
                line_program: None,
                comp_dir: None,
                comp_name: None,
            };
            let abbrevs = hdr.abbreviations(&self.debug_abbrev)?;
            {
                let mut entries = hdr.entries(&abbrevs);
                let mut depth = 0;
                while let Some((change, entry)) = entries.next_dfs()? {
                    depth += change;
                    if entry.tag() == gimli::DW_TAG_compile_unit || entry.tag() == gimli::DW_TAG_type_unit {
                        unit.base_address = match entry.attr_value(gimli::DW_AT_low_pc)? {
                            Some(gimli::AttributeValue::Addr(address)) => address,
                            _ => 0,
                        };
                        unit.comp_dir = entry
                            .attr(gimli::DW_AT_comp_dir)?
                        .and_then(|attr| attr.string_value(&self.debug_str));
                        unit.comp_name = entry
                            .attr(gimli::DW_AT_name)?
                        .and_then(|attr| attr.string_value(&self.debug_str));
                        unit.line_program = match entry.attr_value(gimli::DW_AT_stmt_list)? {
                            Some(gimli::AttributeValue::DebugLineRef(offset)) => self.debug_line
                                .program(
                                    offset,
                                    unit.address_size,
                                    unit.comp_dir.clone(),
                                    unit.comp_name.clone(),
                                )
                                .ok(),
                            _ => None,
                        }
                    }
                    let name = entry.attr(gimli::DW_AT_name).ok().map(|a| {
                        a.and_then(|a| a.string_value(&self.debug_str))
                            .and_then(|v| v.to_string_lossy().ok().map(|s| s.into_owned()))
                    }).unwrap_or_else(|| Some(String::new()))
                        .unwrap_or_else(|| String::new());
                    dies.push(Die {
                        cu: cu_index,
                        offset: entry.offset().to_debug_info_offset(&hdr).0,
                        depth: depth as usize,
                        tag: entry.tag(),
                        name,
                    });
                }
            }
            let offset = hdr.offset().0 + hdr.length_including_self();
            self.units.push((hdr, unit));
            let percent = ((offset as f64) / (self.debug_info_len as f64) * 100.0) as u32;
            cb(self.units.len(), dies.len(), percent);
        }
        Ok((self.units.len(), dies))
    }

    pub fn get_die_info(&mut self, cu_index: usize, offset: usize) -> Result<String, Error> {
        let &(ref cu, ref unit) = &self.units[cu_index];
        let abbrevs = cu.abbreviations(&self.debug_abbrev)?;
        let offset = gimli::DebugInfoOffset(offset).to_unit_offset(&cu)
            .context("DIE is not within its CU!")?;
        let mut entries = cu.entries_at_offset(&abbrevs, offset)?;


        let (_, entry) = entries.next_dfs()?.context("Missing DIE!")?;
        let mut output = String::new();
        write!(&mut output, "{}\n", entry.tag())?;
        let mut attrs = entry.attrs();
        while let Some(attr) = attrs.next().unwrap() {
            write!(&mut output, "  {:32} ", attr.name())?;
            dump_attr_value(&mut output, &attr, unit, &self.debug_str)?;
        }
        Ok(output)
    }
}

/// Format `duration` as seconds with a fractional component.
fn fmt_duration_as_secs(duration: &Duration) -> String
{
    format!("{}.{:03} s", duration.as_secs(), duration.subsec_nanos() / 1000_000)
}

enum Message {
    GetDie {
        cu: usize,
        offset: usize,
    },
}

fn do_background_work(f: &object::File, sink: Sender<Box<CbFunc>>,
                      rx: Receiver<Message>) -> Result<(), Error> {
    let endian = if f.is_little_endian() {
        gimli::RunTimeEndian::Little
    } else {
        gimli::RunTimeEndian::Big
    };
    let start = Instant::now();
    let mut data = read_file(f, endian)?;
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
    while let Ok(msg) = rx.recv() {
        match msg {
            Message::GetDie { cu, offset } => {
                let res = data.get_die_info(cu, offset)
                    .unwrap_or_else(|e| format!("Error: {}", e));
                drop(sink.send(Box::new(move |s: &mut Cursive| {
                    s.add_layer(Dialog::around(TextView::new(res))
                        .title(format!("DIE at {:#08x}", offset))
                        .dismiss_button("Close"));
                })));
            }
        }
    }
    Ok(())
}

pub fn main() -> Result<(), Error> {
    let filename = env::args_os().nth(1).ok_or_else(|| format_err!("Usage: dwarfinspect <file>"))?;
    let mut siv = Cursive::default();
    siv.set_fps(10);
    let sink = siv.cb_sink().clone();
    let (tx, rx) = mpsc::channel();
    // Load the DWARF on a background thread just so we can display the initial UI while loading.
    let child = thread::spawn(move || {
        with_file(&filename, move |f| do_background_work(f, sink, rx))
    });

    siv.add_global_callback('q', |s| s.quit());
    let tx2 = tx.clone();
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
                    .on_submit(move |s: &mut Cursive, _row, index| {
                        s.call_on_id("table", |view: &mut TableView<Die, DieColumn>| {
                            view.borrow_item(index).map(|die| {
                                drop(tx2.send(Message::GetDie {
                                    cu: die.cu,
                                    offset: die.offset,
                                }));
                            });
                        });
                    })
                    .with_id("table")
                    .full_width()
                    .full_height()
            )
            .child(
                TextView::new("Loading...")
                    .effect(Effect::Reverse)
                    .with_id("status")
                    .full_width()
                    .max_height(1)
            )));
    siv.run();
    drop(tx);
    drop(siv);
    drop(child.join());
    Ok(())
}
