use crate::SourceFileEntry;
use crate::vhdl::nodes_def::{Kind as VhdKind, Node as VhdNode};

mod nodes_def;
mod types;

use nodes_def::{Node, Kind};

extern "C" {
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

pub fn export_file(file: Node) -> VhdNode {
    //  Create the vhdl file and set locations
    let vh_file = VhdNode::new(VhdKind::Design_File);
    let loc = file.location();
    let sfe = loc.to_file();
    vh_file.set_location(loc);
    vh_file.set_design_file_source(sfe);
    vh_file.set_design_file_filename(sfe.file_name());
    vh_file.set_design_file_directory(sfe.directory_name());
    let mut last = VhdNode::NULL;

    //  TODO: check ieee.std_logic_1164 is loaded
    //  Convert modules
    let mut vln = file.descriptions();
    while vln != Node::NULL {
        if vln.kind() == Kind::Module {
            //  Create the design unit
            let unit = VhdNode::new(VhdKind::Design_Unit);
            unit.set_location(vln.location());
            unit.set_design_file(vh_file);
            unit.set_identifier(vln.identifier());
            unit.set_date(4); // TODO: convert Date constants
            unit.set_date_state(crate::vhdl::nodes_def::DateStateType::Extern);

            // Create the entity
            let ent = VhdNode::new(VhdKind::Entity_Declaration);
            ent.set_location(vln.location());
            unit.set_library_unit(ent);
            ent.set_design_unit(unit);
            ent.set_identifier(vln.identifier());

            //  Append design unit
            if last == VhdNode::NULL {
                vh_file.set_first_design_unit(unit);
            }
            else {
                last.set_chain(unit)
            }
            last = unit;
        }
        vln = vln.chain();
    }
    vh_file.set_last_design_unit(last);
    return vh_file;
}