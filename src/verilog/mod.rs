use crate::SourceFileEntry;

mod nodes_def;
mod nodes_utils;
mod standard_def;
mod types;
pub mod exporters;

use nodes_def::Node;

extern "C" {
    #[link_name = "verilog__flags__flag_keep_parentheses"]
    pub static mut flag_keep_parentheses: bool;

    #[link_name = "verilog__errors__initialize"]
    pub fn errors_initialize();

    #[link_name = "verilog__scans__init_paths"]
    pub fn scans_init_paths();

    #[link_name = "verilog__sem_scopes__init"]
    pub fn sem_scopes_init();

    #[link_name = "verilog__sem_types__create_basetypes"]
    pub fn sem_types_create_basetypes();

    #[link_name = "verilog__vpi__initialize"]
    pub fn vpi_initialize();

    #[link_name = "verilog__parse__parse_file"]
    pub fn parse_file(sfe: SourceFileEntry) -> Node;

    #[link_name = "verilog__sem__sem_compilation_unit"]
    pub fn sem_compilation_unit(unit: Node);
}
