use crate::std_names;
use crate::vhdl::nodes_def::{
    DirectionType, Flist as VhdFlist, Kind as VhdKind, Mode as VhdMode, Node as VhdNode,
};
use crate::vhdl::nodes_utils::Chain as VhdChain;
use crate::SourceFileEntry;

mod nodes_def;
mod types;
mod nodes_utils;

use nodes_def::{BinaryOps, Kind, Node};
use nodes_utils::Chain;

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

fn convert_binary_op(expr: Node, kind: VhdKind) -> VhdNode {
    let res = VhdNode::new(kind);
    res.set_left(convert_expr(expr.left()));
    res.set_right(convert_expr(expr.right()));
    return res;
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
        Kind::Binary_Op => match expr.binary_op() {
            BinaryOps::Sub => convert_binary_op(expr, VhdKind::Substraction_Operator),
            BinaryOps::Umul => convert_binary_op(expr, VhdKind::Multiplication_Operator),
            _ => {
                eprint!(
                    "convert_binary_op: cannot handle {}\n",
                    BinaryOps::IMAGES[expr.binary_op() as usize]
                );
                return VhdNode::NULL;
            }
        },
        Kind::Name => {
            let res = VhdNode::new(VhdKind::Simple_Name);
            res.set_location(expr.location());
            res.set_identifier(expr.identifier());
            return res;
        }
        Kind::Parenthesis_Expr => {
            let res = VhdNode::new(VhdKind::Parenthesis_Expression);
            res.set_location(expr.location());
            res.set_expression(convert_expr(expr.expression()));
            return res;
        }
        _ => {
            eprint!(
                "convert_expr: cannot handle {}\n",
                Kind::IMAGES[expr.kind() as usize]
            );
            VhdNode::NULL
        }
    }
}
fn convert_type(dtype: Node) -> VhdNode {
    if dtype == Node::NULL {
        let base = VhdNode::new(VhdKind::Simple_Name);
        base.set_identifier(std_names::STRING);
        return base;
    }
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
        _ => {
            eprint!(
                "convert_type: cannot handle {}\n",
                Kind::IMAGES[dtype.kind() as usize]
            );
        }
    }
    VhdNode::NULL
}

fn convert_decl(decl: Node, kind: VhdKind, mode: VhdMode, parent: VhdNode, chain: &mut VhdChain) {
    let res = VhdNode::new(kind);
    res.set_location(decl.location());
    res.set_identifier(decl.identifier());
    res.set_mode(mode);
    if kind != VhdKind::Interface_Constant_Declaration {
        res.set_has_mode(true);
    }
    res.set_parent(parent);

    let dtype = decl.data_type();
    let mut vtype = VhdNode::NULL;
    if decl.kind() == Kind::Parameter {
        let expr = decl.expression();
        if expr != Node::NULL {
            if dtype == Node::NULL {
                match expr.kind() {
                    Kind::Number => {
                        vtype = VhdNode::new(VhdKind::Simple_Name);
                        vtype.set_identifier(std_names::INTEGER);
                    }
                    _ => {}
                }
            }
            res.set_default_value(convert_expr(expr));
        }
    } else {
        vtype = convert_type(dtype);
    }
    res.set_subtype_indication(vtype);

    chain.append(res);
}

fn convert_decls(decls: Node, parent: VhdNode, generics: &mut VhdChain, ports: &mut VhdChain) {
    let mut n = decls;
    while n != Node::NULL {
        match n.kind() {
            Kind::Parameter => {
                convert_decl(
                    n,
                    VhdKind::Interface_Constant_Declaration,
                    VhdMode::In_Mode,
                    parent,
                    generics,
                );
            }
            Kind::Input => {
                convert_decl(
                    n,
                    VhdKind::Interface_Signal_Declaration,
                    VhdMode::In_Mode,
                    parent,
                    ports,
                );
            }
            Kind::Output => {
                convert_decl(
                    n,
                    VhdKind::Interface_Signal_Declaration,
                    VhdMode::Out_Mode,
                    parent,
                    ports,
                );
            }
            Kind::Wire => {}
            _ => {
                eprint!("unhandled {}\n", Kind::IMAGES[n.kind() as usize]);
            }
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
    let mut chain = Chain::new();

    //  Convert modules
    let mut vln = file.descriptions();
    while vln != Node::NULL {
        if vln.kind() == Kind::Module {
            //  Create a component
            let comp = VhdNode::new(VhdKind::Component_Declaration);
            comp.set_location(vln.location());
            comp.set_identifier(vln.identifier());
            comp.set_end_has_reserved_id(true);

            convert_module(vln, comp);

            chain.append(comp);
        }
        vln = vln.chain();
    }
    return chain.head();
}
