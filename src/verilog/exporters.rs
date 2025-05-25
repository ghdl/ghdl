use crate::std_names;
use crate::types::Logic32;
use crate::vhdl::nodes_def::{
    DirectionType, Flist as VhdFlist, Kind as VhdKind, Mode as VhdMode, Node as VhdNode,
};
use crate::vhdl::nodes_utils::Chain as VhdChain;
use crate::str_table;

use crate::verilog::nodes_def;
use crate::verilog::standard_def;

use nodes_def::{BinaryOps, Kind, Node};

//  The universal representation of a type (common in verilog and vhdl)
#[derive(Copy, Clone)]
enum TypeKind {
    //  A vector of known length
    Vec(u32),

    //  An integer
    Int,

    //  Also possible (but not used):
    //  A string
    //  A real
}

impl TypeKind {
    fn from_vtype(vtype: VhdNode) -> Self {
        if vtype.kind() == VhdKind::Array_Subtype_Definition {
            //  Extract the length.
            let rng = vtype.index_constraint_list().get(0);
            if rng.kind() == VhdKind::Range_Expression {
                let l = rng.left_limit_expr();
                let r = rng.right_limit_expr();
                if l.kind() == VhdKind::Integer_Literal && r.kind() == VhdKind::Integer_Literal {
                    let lv = l.value();
                    let rv = r.value();
                    match rng.direction() {
                        DirectionType::To => {
                            if rv >= lv { return TypeKind::Vec((rv - lv + 1) as u32)}
                        }
                        DirectionType::Downto => {
                            if rv <= lv { return TypeKind::Vec((lv - rv + 1) as u32)}
                        }
                    }
                }
            }
            TypeKind::Vec(0)
        } else {
            TypeKind::Int
        }
    }
}

//  Helpers
fn build_vhdl_int(val: i64) -> VhdNode {
    let res = VhdNode::new(VhdKind::Integer_Literal);
    res.set_value(val);
    res
}

fn build_vhdl_name(id: crate::NameId) -> VhdNode {
    let base = VhdNode::new(VhdKind::Simple_Name);
    base.set_identifier(id);
    base
}

fn build_vhdl_slv(loc: crate::files_map::Location, left: VhdNode, right: VhdNode) -> VhdNode {
    let res = VhdNode::new(VhdKind::Array_Subtype_Definition);
    res.set_location(loc);

    //  std_logic_vector type mark.  Do not set location as it is not in
    //  the sources.
    res.set_subtype_type_mark(build_vhdl_name(std_names::STD_LOGIC_VECTOR));

    let indices = VhdFlist::new(1);

    let rng = VhdNode::new(VhdKind::Range_Expression);
    rng.set_left_limit_expr(left);
    rng.set_right_limit_expr(right);
    rng.set_direction(DirectionType::Downto);

    indices.set(0, rng);
    res.set_index_constraint_list(indices);
    res.set_has_array_constraint_flag(true);

    res
}

fn convert_binary_op(expr: Node, op_kind: VhdKind, tkind: TypeKind) -> VhdNode {
    let res = VhdNode::new(op_kind);
    res.set_left(convert_expr(expr.left(), tkind));
    res.set_right(convert_expr(expr.right(), tkind));
    return res;
}

//  Convert a verilog Number (or Big_Number) to a string literal
fn number_to_vec(expr : Node, tlen: u32) -> VhdNode {
    let elen = expr.number_size();
    let len = if tlen != 0 { tlen } else { elen };

    let res = VhdNode::new(VhdKind::String_Literal8);
    res.set_string_length(len as i32);

    let str = str_table::String8Id::new();
    res.set_string8_id(str);

    let mut w : Logic32 = Logic32{val: 0, zx: 0};

    for i in 0..len {
        let idx = if i >= elen && elen != 0 { elen - 1 } else { i };
        if idx % 32 == 0 && i == idx {
            if expr.kind() == Kind::Number {
                w = if i == 0 {
                    Logic32{zx: expr.number_lo_zx(), val: expr.number_lo_val()}
                }
                else {
                    Logic32{zx: expr.number_hi_zx(), val: expr.number_hi_val()}
                }
            }
            else {
                let bn = expr.bignum_index();
                w = bn.logic32(i / 32);
            }
        }
        let b_zx = (w.zx >> (idx % 32)) & 1;
        let b_va = (w.val >> (idx % 32)) & 1;
        let map = [ '0' as u8, '1' as u8, 'Z' as u8, 'X' as u8];
        str.append(map[((b_zx << 1) + b_va) as usize]);
    }
    res
}

fn convert_expr(expr: Node, tkind: TypeKind) -> VhdNode {
    match expr.kind() {
        Kind::Number => match tkind {
            TypeKind::Int => {
                if expr.number_hi_val() != 0 {
                    eprint!("convert_expr: unhandled large number\n");
                } else if expr.number_lo_zx() != 0 || expr.number_hi_zx() != 0 {
                    eprint!("convert_expr: unhandled Z/X number\n");
                }
                build_vhdl_int(expr.number_lo_val() as i64)
            }
            TypeKind::Vec(tlen) => {
                number_to_vec(expr, tlen)
            }
        },
        Kind::Bignum => match tkind {
            TypeKind::Int => {
                eprint!("convert_expr: unhandled big_number as int\n");
                build_vhdl_int(0)
            }
            TypeKind::Vec(tlen) => {
                number_to_vec(expr, tlen)
            }
        },
        Kind::Binary_Op => match expr.binary_op() {
            BinaryOps::Sub => convert_binary_op(expr, VhdKind::Substraction_Operator, tkind),
            BinaryOps::Umul => convert_binary_op(expr, VhdKind::Multiplication_Operator, tkind),
            _ => {
                eprint!(
                    "convert_binary_op: cannot handle {}\n",
                    BinaryOps::IMAGES[expr.binary_op() as usize]
                );
                return VhdNode::NULL;
            }
        },
        Kind::String_Literal => {
            let res = VhdNode::new(VhdKind::String_Literal8);
            res.set_location(expr.location());
            res.set_string8_id(expr.string_id());
            res.set_string_length(expr.string_size() as i32);
            return res;
        }
        Kind::Name => {
            let res = VhdNode::new(VhdKind::Simple_Name);
            res.set_location(expr.location());
            res.set_identifier(expr.identifier());
            return res;
        }
        Kind::Parenthesis_Expr => {
            let res = VhdNode::new(VhdKind::Parenthesis_Expression);
            res.set_location(expr.location());
            res.set_expression(convert_expr(expr.expression(), tkind));
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

fn convert_predefined_typedef(dtype: Node) -> VhdNode {
    match dtype.raw_id() {
        standard_def::IMPLICIT_TYPEDEF => build_vhdl_name(std_names::STD_LOGIC),
        _ => {
            eprint!(
                "convert_predefined_typedef: cannot handle {}\n",
                dtype.raw_id()
            );
            build_vhdl_name(std_names::STRING)
        }
    }
}

fn convert_type(dtype: Node) -> VhdNode {
    if dtype == Node::NULL {
        // No parameter type, assume string.
        return build_vhdl_name(std_names::STRING);
    }
    match dtype.kind() {
        Kind::Packed_Array => build_vhdl_slv(
            dtype.location(),
            convert_expr(dtype.msb(), TypeKind::Int),
            convert_expr(dtype.lsb(), TypeKind::Int),
        ),
        Kind::Predefined_Typedef => convert_predefined_typedef(dtype),
        _ => {
            eprint!(
                "convert_type: cannot handle {}\n",
                Kind::IMAGES[dtype.kind() as usize],
            );
            build_vhdl_name(std_names::STRING)
        }
    }
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
        // For parameters: handle the expression
        let expr = decl.expression();
        if expr != Node::NULL {
            if dtype == Node::NULL {
                // And extract type from the expression if there is no type
                match expr.kind() {
                    Kind::Number => {
                        vtype = build_vhdl_name(std_names::INTEGER);
                    }
                    Kind::String_Literal => {
                        vtype = build_vhdl_name(std_names::STRING);
                    }
                    Kind::Bignum => {
                        let sz = expr.number_size();
                        if sz != 0 {
                            vtype = build_vhdl_slv(
                                expr.location(),
                                build_vhdl_int((sz - 1) as i64),
                                build_vhdl_int(0),
                            );
                        } else {
                            eprintln!("unhandled unsized bignum");
                        }
                    }
                    _ => {
                        eprint!(
                            "convert_decl: cannot handle expression {} for {}\n",
                            Kind::IMAGES[expr.kind() as usize],
                            decl.identifier().to_string(),
                        );
                        //  Use a string when the type is unknown
                        vtype = build_vhdl_name(std_names::STRING);
                    }
                }
            } else {
                vtype = convert_type(dtype);
            }
            let tkind = TypeKind::from_vtype(vtype);
            res.set_default_value(convert_expr(expr, tkind));
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
            Kind::Inout => {
                convert_decl(
                    n,
                    VhdKind::Interface_Signal_Declaration,
                    VhdMode::Inout_Mode,
                    parent,
                    ports,
                );
            }
            Kind::Wire | Kind::Wire_Direct => {}
            Kind::Var => {}
            Kind::Assign | Kind::Always | Kind::Initial => {}
            Kind::Localparam => {}
            Kind::Generate_Region => {}
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

pub fn export_file(file: Node) -> VhdChain {
    let mut chain = VhdChain::new();

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
    chain
}
