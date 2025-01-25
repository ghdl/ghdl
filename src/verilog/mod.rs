use crate::SourceFileEntry;
use crate::std_names;
use crate::vhdl::nodes_def::{Kind as VhdKind, Node as VhdNode, Mode as VhdMode, DirectionType, Flist as VhdFlist};
use crate::vhdl::nodes_utils::Chain as VhdChain;

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

fn convert_expr(expr: Node) -> VhdNode {
    match expr.kind() {
        Kind::Number => {
            let res = VhdNode::new(VhdKind::Integer_Literal);
            if expr.number_hi_val() != 0 || expr.number_lo_zx() != 0 || expr.number_hi_zx() != 0 {
                eprint!("convert_expr: unhandled large number\n");
            }
            res.set_value(expr.number_lo_val() as i64);
            res
        }
        _ => { eprint!("convert_expr: cannot handle {}\n", Kind::IMAGES[expr.kind() as usize]);
        VhdNode::NULL
        }
    }
}
fn convert_type(dtype: Node, parent: VhdNode) -> VhdNode {
    match dtype.kind() {
        Kind::Packed_Array => {
            let loc = dtype.location();
            let res = VhdNode::new(VhdKind::Array_Subtype_Definition);
            res.set_location(loc);

            //  std_logic_vector type mark.  Do not set location as it is not in
            //  the sources.
            let base = VhdNode::new(VhdKind::Simple_Name);
            base.set_identifier(std_names::STD_LOGIC_VECTOR);
            res.set_subtype_type_mark(base);

            let indices = VhdFlist::new(1);

            let rng = VhdNode::new(VhdKind::Range_Expression);
            rng.set_left_limit_expr(convert_expr(dtype.msb()));
            rng.set_right_limit_expr(convert_expr(dtype.lsb()));
            rng.set_direction(DirectionType::Downto);

            indices.set(0, rng);
            res.set_index_constraint_list(indices);
            res.set_has_array_constraint_flag(true);

            return res;
        }
        _ => { eprint!("convert_type: cannot handle {}\n", Kind::IMAGES[dtype.kind() as usize]);}
    }
    VhdNode::NULL
}

fn convert_decl(decl: Node, kind: VhdKind, mode: VhdMode, parent: VhdNode, chain: &mut VhdChain) {
    let res = VhdNode::new(kind);
    res.set_location(decl.location());
    res.set_identifier(decl.identifier());
    res.set_mode(mode);
    res.set_has_mode(true);
    res.set_parent(parent);
    res.set_subtype_indication(convert_type(decl.data_type(), parent));

    chain.append(res);
}

fn convert_decls(decls: Node, parent: VhdNode, generics: &mut VhdChain, ports: &mut VhdChain) {
    let mut n = decls;
    while n != Node::NULL {
        match n.kind() {
            Kind::Parameter => {
                convert_decl(n, VhdKind::Interface_Constant_Declaration, VhdMode::In_Mode, parent, generics);
            }
            Kind::Input => {
                convert_decl(n, VhdKind::Interface_Signal_Declaration, VhdMode::In_Mode, parent, ports);
            }
            Kind::Output => {
                convert_decl(n, VhdKind::Interface_Signal_Declaration, VhdMode::Out_Mode, parent, ports);
            }
            Kind::Wire => {}
            _ => {eprint!("unhandled {}\n", Kind::IMAGES[n.kind() as usize]);}
        }
        n = n.chain();
    }
}
fn convert_module(vlg: Node, vhd: VhdNode) {
    let mut generics = VhdChain::new();
    let mut ports = VhdChain::new();

    convert_decls(vlg.parameter_port_chain(), vhd, &mut generics, &mut ports);
    convert_decls(vlg.ports_chain(), vhd, &mut generics, &mut ports);
    convert_decls(vlg.items_chain(), vhd, &mut generics, &mut ports);
    vhd.set_generic_chain(generics.head());
    vhd.set_port_chain(ports.head());
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

            convert_module(vln, ent);

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