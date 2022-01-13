/// Undefined section
pub const SHN_UNDEF: u16 = 0;
/// No relocation applies
pub const SHN_ABS: u16 = 0xffff;

pub struct ElfHeader {
    pub kind: u8,

    // /// Virtual entry point
    // entry: u16
    /// Program header file offset (0 if not present)
    pub phoff: usize,
    /// Section header file offset (0 if not present)
    pub shoff: usize,

    /// Program header entry number
    pub phnum: usize,
    /// Section header entry number
    pub shnum: u16,

    /// Section header tablke index for the string table
    pub shstridx: u16,
}

pub const SHT_NULL: u8 = 0;
pub const SHT_PROGBITS: u8 = 1;
/// link contains the sh idx for the string table  
/// info contains one greater than the last local symbol symtable index
pub const SHT_SYMTAB: u8 = 2;

pub const SHT_STRTAB: u8 = 3;
pub const SHT_NOBITS: u8 = 4;

/// link contains the sh index of the symbol table  
/// info contains the sh index of the section this relocation applies to
// pub const SHT_RELA: u8 = 5;
pub const SHT_REL: u8 = 6;

pub struct SectionHeader {
    pub name: usize,
    pub kind: u8,
    pub addr: u16,
    pub offset: usize,
    pub link: usize,
    pub info: usize,
}

pub const STN_UNDEF: usize = 0;
// Symbol bindings

/// All bindings with [`STB_LOCAL`] precede other bindings in symbol tables
pub const STB_LOCAL: u8 = 0;
/// No two global symbol can have the same name  
/// If a weak and global symbol have the same name, the weak one is ignored
pub const STB_GLOBAL: u8 = 1;
pub const STB_WEAK: u8 = 2;

// Symbol types/kinds
pub const STT_NOTYPE: u8 = 0;
/// Data object (variable, array, etc)
pub const STT_OBJECT: u8 = 1;
/// Function or executable code
pub const STT_FUNC: u8 = 2;
/// Section  
/// This exists for relocation and normally are [`STB_LOCAL`]
pub const STT_SECTION: u8 = 3;
/// File [`STB_LOCAL`], [SymbolEntry's `shndx`](SymbolEntry) = [`SHN_ABS`]  
/// Precedes all other [`STB_LOCAL`] if present
pub const STT_FILE: u8 = 4;
pub struct SymbolEntry {
    pub name: usize,
    /// If relocatable, offset for symbol  
    /// If executable (or shared object, not yet supported), virtual address
    pub value: u16,
    pub size: usize,

    /// Higher 4 bits are binding  
    /// Lower 4 bits are type/kind
    pub info: u8,
    pub shndx: u16,
}


pub struct RelEntry {
	/// Location at which to apply the relocation action.  
	/// For a relocatable file, the value is the byte offset from the begining of the section
	/// to the storage unit affected by the relocation.  
	/// For an executable file or SO, the value is the virtual address of the unit affected by the relocation
	pub offset: u16,
	/// Symbol table index.  
	/// For example, a call instruction’s relocation entry
	/// would hold the symbol table index of the function being called.  
	/// If the index is [`STN_UNDEF`],
	/// the undefined symbol index, the relocation uses `0` as the "symbol value".
	pub sym: u8,
	/// How are we relocating  
	/// see [relocation strategies `R`](rel_strategies)
	pub kind: u8
}

// pub struct RelaEntry {
// 	pub rel: RelEntry,
// 	pub addend: u16
// }

pub use rel_strategies as R;

pub mod rel_strategies {
	//! # Relocation strategies
	//! 
	//! ### Common names used in calculation descriptions
	// //!  * `A`: [`addend`](super::RelaEntry)
	//!  * `B`: Base address at which the object was loaded
	//!  * `G`: offset into the GOT at which the the addr of the symbol will reside
	//!  * `GOT`: address of the GOT
	//!  * `P`: Location of the storage unit to be relocated (derives from [`offset`](super::RelEntry))
	//!  * `S`: Value of the symbol


	/// Do nothing
	pub const NONE: u8 = 0;

	// /// [RELA ONLY] `S + A`
	// pub const ADD: u8 = 1;
	// /// [RELA ONLY] `S + A - P`
	// pub const ADDP: u8 = 2;

	/// `S`  
	/// Set a GOT entry to the symbol address
	pub const GLOB_DAT: u8 = 6;
}

