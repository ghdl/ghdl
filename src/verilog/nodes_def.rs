#![allow(non_camel_case_types, dead_code)]

use crate::verilog::Node;
use crate::types::*;
use crate::verilog::types::*;
#[derive(PartialEq, PartialOrd)]
#[repr(u16)]
pub enum Kind {
    Error,
    Error_Expr,
    Timescale_Directive,
    Timeunits_Declaration,
    Timeunit,
    Timeprecision,
    Logic_Type,
    Bit_Type,
    Real_Type,
    Shortreal_Type,
    Log_Packed_Array_Cst,
    Bit_Packed_Array_Cst,
    Array_Cst,
    Packed_Array,
    Array,
    Struct_Type,
    Packed_Struct_Type,
    Union_Type,
    Packed_Union_Type,
    Queue,
    Queue_Cst,
    Dynamic_Array_Cst,
    Dynamic_Array,
    Associative_Array,
    Associative_Array_Cst,
    Enum_Type,
    String_Type,
    Chandle_Type,
    Event_Type,
    Virtual_Interface,
    Void_Type,
    Error_Type,
    Null_Type,
    Nature,
    Class,
    Instantiated_Class,
    Class_Instance,
    Generic_Class,
    Wildcard_Type,
    Compilation_Unit,
    Foreign_Module,
    Module,
    Primitive,
    Interface_Declaration,
    Package,
    Program_Declaration,
    Port,
    Task,
    Function,
    OOB_Task,
    OOB_Function,
    Extern_Task,
    Extern_Function,
    Import_DPI_Function,
    Export_DPI_Function,
    Export_DPI_Task,
    Clocking,
    Default_Clocking,
    Disable_Iff,
    Specify,
    Property_Declaration,
    Input,
    Inout,
    Output,
    Interface_Port,
    Modport_Port,
    Tf_Input,
    Tf_Inout,
    Tf_Output,
    Tf_Ref,
    Tf_Const_Ref,
    Parameter,
    Type_Parameter,
    Localparam,
    Type_Localparam,
    Var,
    Return_Var,
    This_Var,
    Iterator_Argument,
    Wire_Direct,
    Wire,
    Tri,
    Wand,
    Triand,
    Wor,
    Trior,
    Tri0,
    Tri1,
    Supply0,
    Supply1,
    Uwire,
    Trireg,
    Typedef,
    Typedef_Class,
    Typedef_Struct,
    Typedef_Forward,
    Predefined_Typedef,
    Package_Import,
    Genvar,
    Enum_Name,
    Enum_Range,
    Foreach_Variable,
    Clock_Var,
    Modport,
    Modport_Input,
    Modport_Output,
    Modport_Inout,
    Modport_Ref,
    Modport_Clocking,
    Modport_Import_Tf,
    Modport_Export_Tf,
    Constraint,
    Constraint_Expression,
    Constraint_If,
    Constraint_Foreach,
    Discipline,
    Branch,
    Port_Branch,
    Nature_Attribute,
    Nature_Access,
    Discipline_Domain,
    Discipline_Potential,
    Discipline_Flow,
    Discipline_Attribute,
    From_Range,
    Exclude_Range,
    Assign,
    Decl_Assign,
    Always,
    Always_Comb,
    Always_Latch,
    Always_Ff,
    Initial,
    Final,
    Debug,
    Module_Instance,
    Primitive_Instance,
    Interface_Instance,
    Program_Instance,
    Parameter_Value_Type,
    Parameter_Value_Expr,
    Defparam,
    Generate_Region,
    Loop_Generate,
    If_Generate,
    Case_Generate,
    Generate_Block,
    Array_Generate_Block,
    Indexed_Generate_Block,
    Analog,
    Assert_Property,
    Assume_Property,
    Gate_And,
    Gate_Nand,
    Gate_Or,
    Gate_Nor,
    Gate_Xor,
    Gate_Xnor,
    Gate_Buf,
    Gate_Not,
    Gate_Bufif0,
    Gate_Bufif1,
    Gate_Notif0,
    Gate_Notif1,
    Gate_Nmos,
    Gate_Pmos,
    Gate_Rnmos,
    Gate_Rpmos,
    Gate_Tran,
    Gate_Rtran,
    Gate_Tranif0,
    Gate_Tranif1,
    Gate_Rtranif0,
    Gate_Rtranif1,
    Gate_Cmos,
    Gate_Rcmos,
    Gate_Pullup,
    Gate_Pulldown,
    Default_Skew,
    Clocking_Skew,
    Control_Terminal,
    Input_Terminal,
    Inout_Terminal,
    Output_Terminal,
    Port_Connection,
    Wildcard_Connection,
    Implicit_Connection,
    Default_Connection,
    Seq_Block,
    Par_Block,
    If,
    For,
    While,
    Do_While,
    Foreach,
    Repeat,
    Forever,
    Wait,
    Wait_Fork,
    Trigger,
    Disable,
    Disable_Fork,
    Proc_Assign,
    Proc_Deassign,
    Noblk_Assign,
    Blocking_Assign,
    Unpack_Assign,
    Pack_Assign,
    Pack_Unpack_Assign,
    Assign_Operator,
    Force_Assign,
    Release,
    Case,
    Casex,
    Casez,
    Case_Item,
    Default_Case_Item,
    Subroutine_Call_Stmt,
    Return_Stmt,
    Break_Stmt,
    Continue_Stmt,
    Label_Stmt,
    Simple_Immediate_Assert,
    Argument,
    Contribution,
    Name,
    This_Name,
    Dotted_Name,
    Scoped_Name,
    Interface_Item,
    Modport_Item,
    Wildcard_Name,
    Property_Name,
    Class_Qualified_Name,
    Method_Name,
    Member_Name,
    Hierarchical,
    Number,
    Computed_Number,
    Bignum,
    Unbased_Literal,
    Time_Literal,
    Step_Literal,
    Infinity,
    Real_Number,
    Scale_Number,
    Mintypmax,
    Bit_Select,
    Part_Select,
    Plus_Part_Select,
    Minus_Part_Select,
    Indexed_Name,
    String_Index,
    Associative_Index,
    Slice_Name,
    Part_Select_Cst,
    Plus_Part_Select_Cst,
    Minus_Part_Select_Cst,
    Slice_Name_Cst,
    Member_Select,
    String_Literal,
    Implicit_Event,
    New_Call,
    New_Expression,
    Dynamic_Array_New,
    Parenthesis_Expr,
    Type_Cast,
    Size_Cast,
    Null,
    This,
    Super,
    Default,
    Aggregate_Literal,
    Aggregate_Literal_Cst,
    Aggregate_Element,
    Event_Control,
    Delay_Control,
    Repeat_Control,
    Cycle_Delay,
    Posedge,
    Negedge,
    Or,
    Delay,
    Element,
    Value_Range,
    Stream_Expression,
    Left_Streaming_Expr,
    Right_Streaming_Expr,
    Left_Streaming_Type,
    Right_Streaming_Type,
    Concatenation,
    Membership,
    Replication_Cst,
    Cond_Op,
    Call,
    Array_Method_Call,
    Randomize_Call,
    System_Call,
    Bits_Expr,
    Bits_Type,
    Binary_Op,
    Short_Circuit_Op,
    Unary_Op,
    Post_Increment,
    Pre_Increment,
    Post_Decrement,
    Pre_Decrement,
    Access_Call,
    Conversion,
    Seq_Repeat,
    Seq_Plus_Repeat,
    Seq_Star_Repeat,
    Seq_Star_Concat,
    Seq_Plus_Concat,
    Seq_Const_Concat,
    Seq_Range_Concat,
    Seq_Throughout,
    Seq_Parenthesis,
    Prop_Not,
    Prop_Or,
    Prop_And,
    Prop_Overlap_Imp,
    Prop_Non_Overlap_Imp,
    Prop_Until,
    Specparam,
    Pulse_Control_Specparam,
    Ifnone,
    Timing_Check,
    Par_Path,
    Full_Path,
    Par_Edge_Path,
    Full_Edge_Path,
    Path_Element,
    Path_Delay3,
    Path_Delay6,
    Path_Delay12,
    Member,
    Packed_Member,
    Udp_Combinational_Entry,
    Udp_Sequential_Entry,
    Udp_Level_Symbol,
    Udp_Change_Symbol,
    Attribute,
    Label,
    Goto,
}

impl Kind {
    const VALUES: [Self; 345] = [
        Self::Error,
        Self::Error_Expr,
        Self::Timescale_Directive,
        Self::Timeunits_Declaration,
        Self::Timeunit,
        Self::Timeprecision,
        Self::Logic_Type,
        Self::Bit_Type,
        Self::Real_Type,
        Self::Shortreal_Type,
        Self::Log_Packed_Array_Cst,
        Self::Bit_Packed_Array_Cst,
        Self::Array_Cst,
        Self::Packed_Array,
        Self::Array,
        Self::Struct_Type,
        Self::Packed_Struct_Type,
        Self::Union_Type,
        Self::Packed_Union_Type,
        Self::Queue,
        Self::Queue_Cst,
        Self::Dynamic_Array_Cst,
        Self::Dynamic_Array,
        Self::Associative_Array,
        Self::Associative_Array_Cst,
        Self::Enum_Type,
        Self::String_Type,
        Self::Chandle_Type,
        Self::Event_Type,
        Self::Virtual_Interface,
        Self::Void_Type,
        Self::Error_Type,
        Self::Null_Type,
        Self::Nature,
        Self::Class,
        Self::Instantiated_Class,
        Self::Class_Instance,
        Self::Generic_Class,
        Self::Wildcard_Type,
        Self::Compilation_Unit,
        Self::Foreign_Module,
        Self::Module,
        Self::Primitive,
        Self::Interface_Declaration,
        Self::Package,
        Self::Program_Declaration,
        Self::Port,
        Self::Task,
        Self::Function,
        Self::OOB_Task,
        Self::OOB_Function,
        Self::Extern_Task,
        Self::Extern_Function,
        Self::Import_DPI_Function,
        Self::Export_DPI_Function,
        Self::Export_DPI_Task,
        Self::Clocking,
        Self::Default_Clocking,
        Self::Disable_Iff,
        Self::Specify,
        Self::Property_Declaration,
        Self::Input,
        Self::Inout,
        Self::Output,
        Self::Interface_Port,
        Self::Modport_Port,
        Self::Tf_Input,
        Self::Tf_Inout,
        Self::Tf_Output,
        Self::Tf_Ref,
        Self::Tf_Const_Ref,
        Self::Parameter,
        Self::Type_Parameter,
        Self::Localparam,
        Self::Type_Localparam,
        Self::Var,
        Self::Return_Var,
        Self::This_Var,
        Self::Iterator_Argument,
        Self::Wire_Direct,
        Self::Wire,
        Self::Tri,
        Self::Wand,
        Self::Triand,
        Self::Wor,
        Self::Trior,
        Self::Tri0,
        Self::Tri1,
        Self::Supply0,
        Self::Supply1,
        Self::Uwire,
        Self::Trireg,
        Self::Typedef,
        Self::Typedef_Class,
        Self::Typedef_Struct,
        Self::Typedef_Forward,
        Self::Predefined_Typedef,
        Self::Package_Import,
        Self::Genvar,
        Self::Enum_Name,
        Self::Enum_Range,
        Self::Foreach_Variable,
        Self::Clock_Var,
        Self::Modport,
        Self::Modport_Input,
        Self::Modport_Output,
        Self::Modport_Inout,
        Self::Modport_Ref,
        Self::Modport_Clocking,
        Self::Modport_Import_Tf,
        Self::Modport_Export_Tf,
        Self::Constraint,
        Self::Constraint_Expression,
        Self::Constraint_If,
        Self::Constraint_Foreach,
        Self::Discipline,
        Self::Branch,
        Self::Port_Branch,
        Self::Nature_Attribute,
        Self::Nature_Access,
        Self::Discipline_Domain,
        Self::Discipline_Potential,
        Self::Discipline_Flow,
        Self::Discipline_Attribute,
        Self::From_Range,
        Self::Exclude_Range,
        Self::Assign,
        Self::Decl_Assign,
        Self::Always,
        Self::Always_Comb,
        Self::Always_Latch,
        Self::Always_Ff,
        Self::Initial,
        Self::Final,
        Self::Debug,
        Self::Module_Instance,
        Self::Primitive_Instance,
        Self::Interface_Instance,
        Self::Program_Instance,
        Self::Parameter_Value_Type,
        Self::Parameter_Value_Expr,
        Self::Defparam,
        Self::Generate_Region,
        Self::Loop_Generate,
        Self::If_Generate,
        Self::Case_Generate,
        Self::Generate_Block,
        Self::Array_Generate_Block,
        Self::Indexed_Generate_Block,
        Self::Analog,
        Self::Assert_Property,
        Self::Assume_Property,
        Self::Gate_And,
        Self::Gate_Nand,
        Self::Gate_Or,
        Self::Gate_Nor,
        Self::Gate_Xor,
        Self::Gate_Xnor,
        Self::Gate_Buf,
        Self::Gate_Not,
        Self::Gate_Bufif0,
        Self::Gate_Bufif1,
        Self::Gate_Notif0,
        Self::Gate_Notif1,
        Self::Gate_Nmos,
        Self::Gate_Pmos,
        Self::Gate_Rnmos,
        Self::Gate_Rpmos,
        Self::Gate_Tran,
        Self::Gate_Rtran,
        Self::Gate_Tranif0,
        Self::Gate_Tranif1,
        Self::Gate_Rtranif0,
        Self::Gate_Rtranif1,
        Self::Gate_Cmos,
        Self::Gate_Rcmos,
        Self::Gate_Pullup,
        Self::Gate_Pulldown,
        Self::Default_Skew,
        Self::Clocking_Skew,
        Self::Control_Terminal,
        Self::Input_Terminal,
        Self::Inout_Terminal,
        Self::Output_Terminal,
        Self::Port_Connection,
        Self::Wildcard_Connection,
        Self::Implicit_Connection,
        Self::Default_Connection,
        Self::Seq_Block,
        Self::Par_Block,
        Self::If,
        Self::For,
        Self::While,
        Self::Do_While,
        Self::Foreach,
        Self::Repeat,
        Self::Forever,
        Self::Wait,
        Self::Wait_Fork,
        Self::Trigger,
        Self::Disable,
        Self::Disable_Fork,
        Self::Proc_Assign,
        Self::Proc_Deassign,
        Self::Noblk_Assign,
        Self::Blocking_Assign,
        Self::Unpack_Assign,
        Self::Pack_Assign,
        Self::Pack_Unpack_Assign,
        Self::Assign_Operator,
        Self::Force_Assign,
        Self::Release,
        Self::Case,
        Self::Casex,
        Self::Casez,
        Self::Case_Item,
        Self::Default_Case_Item,
        Self::Subroutine_Call_Stmt,
        Self::Return_Stmt,
        Self::Break_Stmt,
        Self::Continue_Stmt,
        Self::Label_Stmt,
        Self::Simple_Immediate_Assert,
        Self::Argument,
        Self::Contribution,
        Self::Name,
        Self::This_Name,
        Self::Dotted_Name,
        Self::Scoped_Name,
        Self::Interface_Item,
        Self::Modport_Item,
        Self::Wildcard_Name,
        Self::Property_Name,
        Self::Class_Qualified_Name,
        Self::Method_Name,
        Self::Member_Name,
        Self::Hierarchical,
        Self::Number,
        Self::Computed_Number,
        Self::Bignum,
        Self::Unbased_Literal,
        Self::Time_Literal,
        Self::Step_Literal,
        Self::Infinity,
        Self::Real_Number,
        Self::Scale_Number,
        Self::Mintypmax,
        Self::Bit_Select,
        Self::Part_Select,
        Self::Plus_Part_Select,
        Self::Minus_Part_Select,
        Self::Indexed_Name,
        Self::String_Index,
        Self::Associative_Index,
        Self::Slice_Name,
        Self::Part_Select_Cst,
        Self::Plus_Part_Select_Cst,
        Self::Minus_Part_Select_Cst,
        Self::Slice_Name_Cst,
        Self::Member_Select,
        Self::String_Literal,
        Self::Implicit_Event,
        Self::New_Call,
        Self::New_Expression,
        Self::Dynamic_Array_New,
        Self::Parenthesis_Expr,
        Self::Type_Cast,
        Self::Size_Cast,
        Self::Null,
        Self::This,
        Self::Super,
        Self::Default,
        Self::Aggregate_Literal,
        Self::Aggregate_Literal_Cst,
        Self::Aggregate_Element,
        Self::Event_Control,
        Self::Delay_Control,
        Self::Repeat_Control,
        Self::Cycle_Delay,
        Self::Posedge,
        Self::Negedge,
        Self::Or,
        Self::Delay,
        Self::Element,
        Self::Value_Range,
        Self::Stream_Expression,
        Self::Left_Streaming_Expr,
        Self::Right_Streaming_Expr,
        Self::Left_Streaming_Type,
        Self::Right_Streaming_Type,
        Self::Concatenation,
        Self::Membership,
        Self::Replication_Cst,
        Self::Cond_Op,
        Self::Call,
        Self::Array_Method_Call,
        Self::Randomize_Call,
        Self::System_Call,
        Self::Bits_Expr,
        Self::Bits_Type,
        Self::Binary_Op,
        Self::Short_Circuit_Op,
        Self::Unary_Op,
        Self::Post_Increment,
        Self::Pre_Increment,
        Self::Post_Decrement,
        Self::Pre_Decrement,
        Self::Access_Call,
        Self::Conversion,
        Self::Seq_Repeat,
        Self::Seq_Plus_Repeat,
        Self::Seq_Star_Repeat,
        Self::Seq_Star_Concat,
        Self::Seq_Plus_Concat,
        Self::Seq_Const_Concat,
        Self::Seq_Range_Concat,
        Self::Seq_Throughout,
        Self::Seq_Parenthesis,
        Self::Prop_Not,
        Self::Prop_Or,
        Self::Prop_And,
        Self::Prop_Overlap_Imp,
        Self::Prop_Non_Overlap_Imp,
        Self::Prop_Until,
        Self::Specparam,
        Self::Pulse_Control_Specparam,
        Self::Ifnone,
        Self::Timing_Check,
        Self::Par_Path,
        Self::Full_Path,
        Self::Par_Edge_Path,
        Self::Full_Edge_Path,
        Self::Path_Element,
        Self::Path_Delay3,
        Self::Path_Delay6,
        Self::Path_Delay12,
        Self::Member,
        Self::Packed_Member,
        Self::Udp_Combinational_Entry,
        Self::Udp_Sequential_Entry,
        Self::Udp_Level_Symbol,
        Self::Udp_Change_Symbol,
        Self::Attribute,
        Self::Label,
        Self::Goto,
    ];

    const IMAGES: [&'static str; 345] = [
        "error",
        "error_expr",
        "timescale_directive",
        "timeunits_declaration",
        "timeunit",
        "timeprecision",
        "logic_type",
        "bit_type",
        "real_type",
        "shortreal_type",
        "log_packed_array_cst",
        "bit_packed_array_cst",
        "array_cst",
        "packed_array",
        "array",
        "struct_type",
        "packed_struct_type",
        "union_type",
        "packed_union_type",
        "queue",
        "queue_cst",
        "dynamic_array_cst",
        "dynamic_array",
        "associative_array",
        "associative_array_cst",
        "enum_type",
        "string_type",
        "chandle_type",
        "event_type",
        "virtual_interface",
        "void_type",
        "error_type",
        "null_type",
        "nature",
        "class",
        "instantiated_class",
        "class_instance",
        "generic_class",
        "wildcard_type",
        "compilation_unit",
        "foreign_module",
        "module",
        "primitive",
        "interface_declaration",
        "package",
        "program_declaration",
        "port",
        "task",
        "function",
        "oob_task",
        "oob_function",
        "extern_task",
        "extern_function",
        "import_dpi_function",
        "export_dpi_function",
        "export_dpi_task",
        "clocking",
        "default_clocking",
        "disable_iff",
        "specify",
        "property_declaration",
        "input",
        "inout",
        "output",
        "interface_port",
        "modport_port",
        "tf_input",
        "tf_inout",
        "tf_output",
        "tf_ref",
        "tf_const_ref",
        "parameter",
        "type_parameter",
        "localparam",
        "type_localparam",
        "var",
        "return_var",
        "this_var",
        "iterator_argument",
        "wire_direct",
        "wire",
        "tri",
        "wand",
        "triand",
        "wor",
        "trior",
        "tri0",
        "tri1",
        "supply0",
        "supply1",
        "uwire",
        "trireg",
        "typedef",
        "typedef_class",
        "typedef_struct",
        "typedef_forward",
        "predefined_typedef",
        "package_import",
        "genvar",
        "enum_name",
        "enum_range",
        "foreach_variable",
        "clock_var",
        "modport",
        "modport_input",
        "modport_output",
        "modport_inout",
        "modport_ref",
        "modport_clocking",
        "modport_import_tf",
        "modport_export_tf",
        "constraint",
        "constraint_expression",
        "constraint_if",
        "constraint_foreach",
        "discipline",
        "branch",
        "port_branch",
        "nature_attribute",
        "nature_access",
        "discipline_domain",
        "discipline_potential",
        "discipline_flow",
        "discipline_attribute",
        "from_range",
        "exclude_range",
        "assign",
        "decl_assign",
        "always",
        "always_comb",
        "always_latch",
        "always_ff",
        "initial",
        "final",
        "debug",
        "module_instance",
        "primitive_instance",
        "interface_instance",
        "program_instance",
        "parameter_value_type",
        "parameter_value_expr",
        "defparam",
        "generate_region",
        "loop_generate",
        "if_generate",
        "case_generate",
        "generate_block",
        "array_generate_block",
        "indexed_generate_block",
        "analog",
        "assert_property",
        "assume_property",
        "gate_and",
        "gate_nand",
        "gate_or",
        "gate_nor",
        "gate_xor",
        "gate_xnor",
        "gate_buf",
        "gate_not",
        "gate_bufif0",
        "gate_bufif1",
        "gate_notif0",
        "gate_notif1",
        "gate_nmos",
        "gate_pmos",
        "gate_rnmos",
        "gate_rpmos",
        "gate_tran",
        "gate_rtran",
        "gate_tranif0",
        "gate_tranif1",
        "gate_rtranif0",
        "gate_rtranif1",
        "gate_cmos",
        "gate_rcmos",
        "gate_pullup",
        "gate_pulldown",
        "default_skew",
        "clocking_skew",
        "control_terminal",
        "input_terminal",
        "inout_terminal",
        "output_terminal",
        "port_connection",
        "wildcard_connection",
        "implicit_connection",
        "default_connection",
        "seq_block",
        "par_block",
        "if",
        "for",
        "while",
        "do_while",
        "foreach",
        "repeat",
        "forever",
        "wait",
        "wait_fork",
        "trigger",
        "disable",
        "disable_fork",
        "proc_assign",
        "proc_deassign",
        "noblk_assign",
        "blocking_assign",
        "unpack_assign",
        "pack_assign",
        "pack_unpack_assign",
        "assign_operator",
        "force_assign",
        "release",
        "case",
        "casex",
        "casez",
        "case_item",
        "default_case_item",
        "subroutine_call_stmt",
        "return_stmt",
        "break_stmt",
        "continue_stmt",
        "label_stmt",
        "simple_immediate_assert",
        "argument",
        "contribution",
        "name",
        "this_name",
        "dotted_name",
        "scoped_name",
        "interface_item",
        "modport_item",
        "wildcard_name",
        "property_name",
        "class_qualified_name",
        "method_name",
        "member_name",
        "hierarchical",
        "number",
        "computed_number",
        "bignum",
        "unbased_literal",
        "time_literal",
        "step_literal",
        "infinity",
        "real_number",
        "scale_number",
        "mintypmax",
        "bit_select",
        "part_select",
        "plus_part_select",
        "minus_part_select",
        "indexed_name",
        "string_index",
        "associative_index",
        "slice_name",
        "part_select_cst",
        "plus_part_select_cst",
        "minus_part_select_cst",
        "slice_name_cst",
        "member_select",
        "string_literal",
        "implicit_event",
        "new_call",
        "new_expression",
        "dynamic_array_new",
        "parenthesis_expr",
        "type_cast",
        "size_cast",
        "null",
        "this",
        "super",
        "default",
        "aggregate_literal",
        "aggregate_literal_cst",
        "aggregate_element",
        "event_control",
        "delay_control",
        "repeat_control",
        "cycle_delay",
        "posedge",
        "negedge",
        "or",
        "delay",
        "element",
        "value_range",
        "stream_expression",
        "left_streaming_expr",
        "right_streaming_expr",
        "left_streaming_type",
        "right_streaming_type",
        "concatenation",
        "membership",
        "replication_cst",
        "cond_op",
        "call",
        "array_method_call",
        "randomize_call",
        "system_call",
        "bits_expr",
        "bits_type",
        "binary_op",
        "short_circuit_op",
        "unary_op",
        "post_increment",
        "pre_increment",
        "post_decrement",
        "pre_decrement",
        "access_call",
        "conversion",
        "seq_repeat",
        "seq_plus_repeat",
        "seq_star_repeat",
        "seq_star_concat",
        "seq_plus_concat",
        "seq_const_concat",
        "seq_range_concat",
        "seq_throughout",
        "seq_parenthesis",
        "prop_not",
        "prop_or",
        "prop_and",
        "prop_overlap_imp",
        "prop_non_overlap_imp",
        "prop_until",
        "specparam",
        "pulse_control_specparam",
        "ifnone",
        "timing_check",
        "par_path",
        "full_path",
        "par_edge_path",
        "full_edge_path",
        "path_element",
        "path_delay3",
        "path_delay6",
        "path_delay12",
        "member",
        "packed_member",
        "udp_combinational_entry",
        "udp_sequential_entry",
        "udp_level_symbol",
        "udp_change_symbol",
        "attribute",
        "label",
        "goto",
    ];
}
impl Kind {
    fn is_delay_nets(self: Self) -> bool {
        self >= Kind::Wire && self <= Kind::Trireg
    }

    fn is_nets(self: Self) -> bool {
        self >= Kind::Wire_Direct && self <= Kind::Trireg
    }

    fn is_types(self: Self) -> bool {
        self >= Kind::Logic_Type && self <= Kind::Class_Instance
    }

    fn is_scalar_types(self: Self) -> bool {
        self >= Kind::Logic_Type && self <= Kind::Shortreal_Type
    }

    fn is_vector_types(self: Self) -> bool {
        self >= Kind::Log_Packed_Array_Cst && self <= Kind::Bit_Packed_Array_Cst
    }

    fn is_forward_typedef(self: Self) -> bool {
        self >= Kind::Typedef_Class && self <= Kind::Typedef_Forward
    }

    fn is_class(self: Self) -> bool {
        self >= Kind::Class && self <= Kind::Instantiated_Class
    }

    fn is_any_class(self: Self) -> bool {
        self >= Kind::Class && self <= Kind::Generic_Class
    }

    fn is_process(self: Self) -> bool {
        self >= Kind::Always && self <= Kind::Debug
    }

    fn is_net_port(self: Self) -> bool {
        self >= Kind::Input && self <= Kind::Output
    }

    fn is_port(self: Self) -> bool {
        self >= Kind::Input && self <= Kind::Modport_Port
    }

    fn is_module(self: Self) -> bool {
        self >= Kind::Foreign_Module && self <= Kind::Module
    }

    fn is_tf_port(self: Self) -> bool {
        self >= Kind::Tf_Input && self <= Kind::Tf_Const_Ref
    }

    fn is_tf(self: Self) -> bool {
        self >= Kind::Task && self <= Kind::Function
    }

    fn is_oob_tf(self: Self) -> bool {
        self >= Kind::OOB_Task && self <= Kind::OOB_Function
    }

    fn is_any_tf(self: Self) -> bool {
        self >= Kind::Task && self <= Kind::Extern_Function
    }

    fn is_modport_port(self: Self) -> bool {
        self >= Kind::Modport_Input && self <= Kind::Modport_Export_Tf
    }

    fn is_gate(self: Self) -> bool {
        self >= Kind::Gate_And && self <= Kind::Gate_Pulldown
    }

    fn is_input_gate(self: Self) -> bool {
        self >= Kind::Gate_And && self <= Kind::Gate_Xnor
    }

    fn is_output_gate(self: Self) -> bool {
        self >= Kind::Gate_Buf && self <= Kind::Gate_Not
    }

    fn is_enable_gate(self: Self) -> bool {
        self >= Kind::Gate_Bufif0 && self <= Kind::Gate_Notif1
    }

    fn is_mos_switch(self: Self) -> bool {
        self >= Kind::Gate_Nmos && self <= Kind::Gate_Rpmos
    }

    fn is_pass_switch(self: Self) -> bool {
        self >= Kind::Gate_Tran && self <= Kind::Gate_Rtran
    }

    fn is_pass_en_switch(self: Self) -> bool {
        self >= Kind::Gate_Tranif0 && self <= Kind::Gate_Rtranif1
    }

    fn is_cmos_switch(self: Self) -> bool {
        self >= Kind::Gate_Cmos && self <= Kind::Gate_Rcmos
    }

    fn is_pull_gate(self: Self) -> bool {
        self >= Kind::Gate_Pullup && self <= Kind::Gate_Pulldown
    }

    fn is_terminal(self: Self) -> bool {
        self >= Kind::Control_Terminal && self <= Kind::Output_Terminal
    }

    fn is_connection(self: Self) -> bool {
        self >= Kind::Port_Connection && self <= Kind::Default_Connection
    }

    fn is_terminal_or_connection(self: Self) -> bool {
        self >= Kind::Control_Terminal && self <= Kind::Wildcard_Connection
    }

    fn is_case(self: Self) -> bool {
        self >= Kind::Case && self <= Kind::Casez
    }

    fn is_case_item(self: Self) -> bool {
        self >= Kind::Case_Item && self <= Kind::Default_Case_Item
    }

    fn is_inc_dec(self: Self) -> bool {
        self >= Kind::Post_Increment && self <= Kind::Pre_Decrement
    }

    fn is_streaming(self: Self) -> bool {
        self >= Kind::Left_Streaming_Expr && self <= Kind::Right_Streaming_Type
    }

    fn is_part_select(self: Self) -> bool {
        self >= Kind::Part_Select && self <= Kind::Minus_Part_Select
    }

    fn is_indexed_part_select(self: Self) -> bool {
        self >= Kind::Plus_Part_Select && self <= Kind::Minus_Part_Select
    }

    fn is_instance(self: Self) -> bool {
        self >= Kind::Module_Instance && self <= Kind::Program_Instance
    }

    fn is_call(self: Self) -> bool {
        self >= Kind::Call && self <= Kind::Randomize_Call
    }

    fn is_edge(self: Self) -> bool {
        self >= Kind::Posedge && self <= Kind::Negedge
    }

    fn is_seq_expr(self: Self) -> bool {
        self >= Kind::Seq_Repeat && self <= Kind::Seq_Parenthesis
    }

    fn is_prop_expr(self: Self) -> bool {
        self >= Kind::Prop_Not && self <= Kind::Prop_Until
    }

}
#[repr(u8)]
pub enum Violation {
    None,
    Unique,
    Unique0,
    Priority,
}

impl Violation {
    const VALUES: [Self; 4] = [
        Self::None,
        Self::Unique,
        Self::Unique0,
        Self::Priority,
    ];

    const IMAGES: [&'static str; 4] = [
        "none",
        "unique",
        "unique0",
        "priority",
    ];
}
#[repr(u8)]
pub enum Visibility {
    None,
    Public,
    Protected,
    Local,
}

impl Visibility {
    const VALUES: [Self; 4] = [
        Self::None,
        Self::Public,
        Self::Protected,
        Self::Local,
    ];

    const IMAGES: [&'static str; 4] = [
        "none",
        "public",
        "protected",
        "local",
    ];
}
#[repr(u8)]
pub enum DPISpec {
    Unknown,
    DPI_C,
    DPI,
}

impl DPISpec {
    const VALUES: [Self; 3] = [
        Self::Unknown,
        Self::DPI_C,
        Self::DPI,
    ];

    const IMAGES: [&'static str; 3] = [
        "unknown",
        "dpi_c",
        "dpi",
    ];
}
#[repr(u8)]
pub enum Edge {
    None,
    Posedge,
    Negedge,
    Any,
}

impl Edge {
    const VALUES: [Self; 4] = [
        Self::None,
        Self::Posedge,
        Self::Negedge,
        Self::Any,
    ];

    const IMAGES: [&'static str; 4] = [
        "none",
        "posedge",
        "negedge",
        "any",
    ];
}
#[repr(u8)]
pub enum Base {
    Binary,
    Octal,
    Decimal,
    Hexa,
}

impl Base {
    const VALUES: [Self; 4] = [
        Self::Binary,
        Self::Octal,
        Self::Decimal,
        Self::Hexa,
    ];

    const IMAGES: [&'static str; 4] = [
        "binary",
        "octal",
        "decimal",
        "hexa",
    ];
}
#[repr(u8)]
pub enum Lifetime {
    Static,
    Automatic,
}

impl Lifetime {
    const VALUES: [Self; 2] = [
        Self::Static,
        Self::Automatic,
    ];

    const IMAGES: [&'static str; 2] = [
        "static",
        "automatic",
    ];
}
#[repr(u8)]
pub enum Join {
    All,
    Any,
    None,
}

impl Join {
    const VALUES: [Self; 3] = [
        Self::All,
        Self::Any,
        Self::None,
    ];

    const IMAGES: [&'static str; 3] = [
        "all",
        "any",
        "none",
    ];
}
#[repr(u8)]
pub enum UdpSymbol {
    Udp_0,
    Udp_1,
    Udp_X,
    Udp_Qm,
    Udp_B,
    Udp_R,
    Udp_F,
    Udp_P,
    Udp_N,
    Udp_Any,
    Udp_No,
}

impl UdpSymbol {
    const VALUES: [Self; 11] = [
        Self::Udp_0,
        Self::Udp_1,
        Self::Udp_X,
        Self::Udp_Qm,
        Self::Udp_B,
        Self::Udp_R,
        Self::Udp_F,
        Self::Udp_P,
        Self::Udp_N,
        Self::Udp_Any,
        Self::Udp_No,
    ];

    const IMAGES: [&'static str; 11] = [
        "udp_0",
        "udp_1",
        "udp_x",
        "udp_qm",
        "udp_b",
        "udp_r",
        "udp_f",
        "udp_p",
        "udp_n",
        "udp_any",
        "udp_no",
    ];
}
#[repr(u8)]
pub enum Polarity {
    Unknown,
    Positive,
    Negative,
}

impl Polarity {
    const VALUES: [Self; 3] = [
        Self::Unknown,
        Self::Positive,
        Self::Negative,
    ];

    const IMAGES: [&'static str; 3] = [
        "unknown",
        "positive",
        "negative",
    ];
}
#[repr(u8)]
pub enum UdpKind {
    Combinational,
    Level_Sensitive,
    Edge_Sensitive,
}

impl UdpKind {
    const VALUES: [Self; 3] = [
        Self::Combinational,
        Self::Level_Sensitive,
        Self::Edge_Sensitive,
    ];

    const IMAGES: [&'static str; 3] = [
        "combinational",
        "level_sensitive",
        "edge_sensitive",
    ];
}
#[repr(u8)]
pub enum ConvOps {
    None,
    Lv_Zext,
    Lv_Sext,
    Lv_Trunc,
    Lv_Nop,
    Lv_Log,
    Lv_Bit,
    Lv_Bv_Zext,
    Lv_Bv_Sext,
    Lv_Bv_Trunc,
    Lv_Bv,
    Lv_Float,
    Bv_Zext,
    Bv_Sext,
    Bv_Trunc,
    Bv_Nop,
    Bv_Log,
    Bv_Bit,
    Bv_Lv_Zext,
    Bv_Lv_Sext,
    Bv_Lv_Trunc,
    Bv_Lv,
    Bv_Float,
    Log_Bit,
    Log_Slv,
    Log_Ulv,
    Log_Sbv,
    Log_Ubv,
    Log_Real,
    Log_Shortreal,
    Bit_Log,
    Bit_Slv,
    Bit_Ulv,
    Bit_Sbv,
    Bit_Ubv,
    Fp32_Fp64,
    Fp64_Fp32,
    Fp64_Ulv,
    Fp64_Slv,
    Fp64_Ubv,
    Fp64_Sbv,
}

impl ConvOps {
    const VALUES: [Self; 41] = [
        Self::None,
        Self::Lv_Zext,
        Self::Lv_Sext,
        Self::Lv_Trunc,
        Self::Lv_Nop,
        Self::Lv_Log,
        Self::Lv_Bit,
        Self::Lv_Bv_Zext,
        Self::Lv_Bv_Sext,
        Self::Lv_Bv_Trunc,
        Self::Lv_Bv,
        Self::Lv_Float,
        Self::Bv_Zext,
        Self::Bv_Sext,
        Self::Bv_Trunc,
        Self::Bv_Nop,
        Self::Bv_Log,
        Self::Bv_Bit,
        Self::Bv_Lv_Zext,
        Self::Bv_Lv_Sext,
        Self::Bv_Lv_Trunc,
        Self::Bv_Lv,
        Self::Bv_Float,
        Self::Log_Bit,
        Self::Log_Slv,
        Self::Log_Ulv,
        Self::Log_Sbv,
        Self::Log_Ubv,
        Self::Log_Real,
        Self::Log_Shortreal,
        Self::Bit_Log,
        Self::Bit_Slv,
        Self::Bit_Ulv,
        Self::Bit_Sbv,
        Self::Bit_Ubv,
        Self::Fp32_Fp64,
        Self::Fp64_Fp32,
        Self::Fp64_Ulv,
        Self::Fp64_Slv,
        Self::Fp64_Ubv,
        Self::Fp64_Sbv,
    ];

    const IMAGES: [&'static str; 41] = [
        "none",
        "lv_zext",
        "lv_sext",
        "lv_trunc",
        "lv_nop",
        "lv_log",
        "lv_bit",
        "lv_bv_zext",
        "lv_bv_sext",
        "lv_bv_trunc",
        "lv_bv",
        "lv_float",
        "bv_zext",
        "bv_sext",
        "bv_trunc",
        "bv_nop",
        "bv_log",
        "bv_bit",
        "bv_lv_zext",
        "bv_lv_sext",
        "bv_lv_trunc",
        "bv_lv",
        "bv_float",
        "log_bit",
        "log_slv",
        "log_ulv",
        "log_sbv",
        "log_ubv",
        "log_real",
        "log_shortreal",
        "bit_log",
        "bit_slv",
        "bit_ulv",
        "bit_sbv",
        "bit_ubv",
        "fp32_fp64",
        "fp64_fp32",
        "fp64_ulv",
        "fp64_slv",
        "fp64_ubv",
        "fp64_sbv",
    ];
}
#[repr(u8)]
pub enum BinaryOps {
    Unknown,
    Logic_And,
    Logic_Or,
    Logic_Imp,
    Logic_Eqv,
    Ult,
    Slt,
    Ule,
    Sle,
    Ugt,
    Sgt,
    Uge,
    Sge,
    Log_Eq,
    Log_Ne,
    Case_Eq,
    Case_Ne,
    Bit_And,
    Bit_Or,
    Bit_Xor,
    Bit_Xnor,
    Bit_Nxor,
    Add,
    Sub,
    Umul,
    Smul,
    Udiv,
    Sdiv,
    Umod,
    Smod,
    Exp,
    Left_Lshift,
    Right_Lshift,
    Left_Ashift,
    Right_Ashift,
}

impl BinaryOps {
    const VALUES: [Self; 35] = [
        Self::Unknown,
        Self::Logic_And,
        Self::Logic_Or,
        Self::Logic_Imp,
        Self::Logic_Eqv,
        Self::Ult,
        Self::Slt,
        Self::Ule,
        Self::Sle,
        Self::Ugt,
        Self::Sgt,
        Self::Uge,
        Self::Sge,
        Self::Log_Eq,
        Self::Log_Ne,
        Self::Case_Eq,
        Self::Case_Ne,
        Self::Bit_And,
        Self::Bit_Or,
        Self::Bit_Xor,
        Self::Bit_Xnor,
        Self::Bit_Nxor,
        Self::Add,
        Self::Sub,
        Self::Umul,
        Self::Smul,
        Self::Udiv,
        Self::Sdiv,
        Self::Umod,
        Self::Smod,
        Self::Exp,
        Self::Left_Lshift,
        Self::Right_Lshift,
        Self::Left_Ashift,
        Self::Right_Ashift,
    ];

    const IMAGES: [&'static str; 35] = [
        "unknown",
        "logic_and",
        "logic_or",
        "logic_imp",
        "logic_eqv",
        "ult",
        "slt",
        "ule",
        "sle",
        "ugt",
        "sgt",
        "uge",
        "sge",
        "log_eq",
        "log_ne",
        "case_eq",
        "case_ne",
        "bit_and",
        "bit_or",
        "bit_xor",
        "bit_xnor",
        "bit_nxor",
        "add",
        "sub",
        "umul",
        "smul",
        "udiv",
        "sdiv",
        "umod",
        "smod",
        "exp",
        "left_lshift",
        "right_lshift",
        "left_ashift",
        "right_ashift",
    ];
}
#[repr(u8)]
pub enum UnaryOps {
    Plus,
    Minus,
    Bit_Neg,
    Logic_Neg,
    Red_Or,
    Red_Nor,
    Red_And,
    Red_Nand,
    Red_Xor,
    Red_Xnor,
    Red_Nxor,
}

impl UnaryOps {
    const VALUES: [Self; 11] = [
        Self::Plus,
        Self::Minus,
        Self::Bit_Neg,
        Self::Logic_Neg,
        Self::Red_Or,
        Self::Red_Nor,
        Self::Red_And,
        Self::Red_Nand,
        Self::Red_Xor,
        Self::Red_Xnor,
        Self::Red_Nxor,
    ];

    const IMAGES: [&'static str; 11] = [
        "plus",
        "minus",
        "bit_neg",
        "logic_neg",
        "red_or",
        "red_nor",
        "red_and",
        "red_nand",
        "red_xor",
        "red_xnor",
        "red_nxor",
    ];
}
pub type SysTfId = u32;

extern "C" {
  #[link_name = "verilog__nodes__get_kind"]
  fn get_kind(n: Node) -> Kind;

  #[link_name = "verilog__nodes__get_location"]
  fn get_location(n: Node) -> u32;

  #[link_name = "verilog__nodes__get_parent"]
  fn get_parent(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_parent"]
  fn set_parent(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_call_scope"]
  fn get_call_scope(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_call_scope"]
  fn set_call_scope(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_identifier"]
  fn get_identifier(n: Node) -> NameId;

  #[link_name = "verilog__nodes__set_identifier"]
  fn set_identifier(n: Node, v: NameId);

  #[link_name = "verilog__nodes__get_c_identifier"]
  fn get_c_identifier(n: Node) -> NameId;

  #[link_name = "verilog__nodes__set_c_identifier"]
  fn set_c_identifier(n: Node, v: NameId);

  #[link_name = "verilog__nodes__get_ports_chain"]
  fn get_ports_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_ports_chain"]
  fn set_ports_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_tf_ports_chain"]
  fn get_tf_ports_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_tf_ports_chain"]
  fn set_tf_ports_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_package_import_chain"]
  fn get_package_import_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_package_import_chain"]
  fn set_package_import_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_parameter_port_chain"]
  fn get_parameter_port_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_parameter_port_chain"]
  fn set_parameter_port_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_parameter"]
  fn get_parameter(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_parameter"]
  fn set_parameter(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_foreign_node"]
  fn get_foreign_node(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_foreign_node"]
  fn set_foreign_node(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_descriptions"]
  fn get_descriptions(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_descriptions"]
  fn set_descriptions(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_class_item_chain"]
  fn get_class_item_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_class_item_chain"]
  fn set_class_item_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_package_item_chain"]
  fn get_package_item_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_package_item_chain"]
  fn set_package_item_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_items_chain"]
  fn get_items_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_items_chain"]
  fn set_items_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_clocking_item_chain"]
  fn get_clocking_item_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_clocking_item_chain"]
  fn set_clocking_item_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_tf_item_declaration_chain"]
  fn get_tf_item_declaration_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_tf_item_declaration_chain"]
  fn set_tf_item_declaration_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_block_item_declaration_chain"]
  fn get_block_item_declaration_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_block_item_declaration_chain"]
  fn set_block_item_declaration_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_generate_item_chain"]
  fn get_generate_item_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_generate_item_chain"]
  fn set_generate_item_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_specify_item_chain"]
  fn get_specify_item_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_specify_item_chain"]
  fn set_specify_item_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_statements_chain"]
  fn get_statements_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_statements_chain"]
  fn set_statements_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_modport_ports_chain"]
  fn get_modport_ports_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_modport_ports_chain"]
  fn set_modport_ports_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_chain"]
  fn get_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_chain"]
  fn set_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_constraint_block_chain"]
  fn get_constraint_block_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_constraint_block_chain"]
  fn set_constraint_block_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_constraint_set"]
  fn get_constraint_set(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_constraint_set"]
  fn set_constraint_set(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_oob_prefix"]
  fn get_oob_prefix(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_oob_prefix"]
  fn set_oob_prefix(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_out_of_block_declaration"]
  fn get_out_of_block_declaration(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_out_of_block_declaration"]
  fn set_out_of_block_declaration(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_generate_index"]
  fn get_generate_index(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_generate_index"]
  fn set_generate_index(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_return_variable"]
  fn get_return_variable(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_return_variable"]
  fn set_return_variable(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_return_variable_ref"]
  fn get_return_variable_ref(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_return_variable_ref"]
  fn set_return_variable_ref(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_this_variable"]
  fn get_this_variable(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_this_variable"]
  fn set_this_variable(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_expression"]
  fn get_expression(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_expression"]
  fn set_expression(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_reject_limit"]
  fn get_reject_limit(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_reject_limit"]
  fn set_reject_limit(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_error_limit"]
  fn get_error_limit(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_error_limit"]
  fn set_error_limit(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_sequence"]
  fn get_sequence(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_sequence"]
  fn set_sequence(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_init_expression"]
  fn get_init_expression(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_init_expression"]
  fn set_init_expression(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_size_expression"]
  fn get_size_expression(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_size_expression"]
  fn set_size_expression(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_override_stmt"]
  fn get_override_stmt(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_override_stmt"]
  fn set_override_stmt(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_parameter_expression"]
  fn get_parameter_expression(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_parameter_expression"]
  fn set_parameter_expression(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_parameter_type"]
  fn get_parameter_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_parameter_type"]
  fn set_parameter_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_value_range"]
  fn get_value_range(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_value_range"]
  fn set_value_range(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_lsb_include_flag"]
  fn get_lsb_include_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_lsb_include_flag"]
  fn set_lsb_include_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_msb_include_flag"]
  fn get_msb_include_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_msb_include_flag"]
  fn set_msb_include_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_range"]
  fn get_range(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_range"]
  fn set_range(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_msb"]
  fn get_msb(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_msb"]
  fn set_msb(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_lsb"]
  fn get_lsb(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_lsb"]
  fn set_lsb(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_msb_cst"]
  fn get_msb_cst(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_msb_cst"]
  fn set_msb_cst(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_lsb_cst"]
  fn get_lsb_cst(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_lsb_cst"]
  fn set_lsb_cst(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_base_expr"]
  fn get_base_expr(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_base_expr"]
  fn set_base_expr(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_width_expr"]
  fn get_width_expr(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_width_expr"]
  fn set_width_expr(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_width_cst"]
  fn get_width_cst(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_width_cst"]
  fn set_width_cst(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_type_width"]
  fn get_type_width(n: Node) -> Width;

  #[link_name = "verilog__nodes__set_type_width"]
  fn set_type_width(n: Node, v: Width);

  #[link_name = "verilog__nodes__get_type_size"]
  fn get_type_size(n: Node) -> Tsize;

  #[link_name = "verilog__nodes__set_type_size"]
  fn set_type_size(n: Node, v: Tsize);

  #[link_name = "verilog__nodes__get_stride_width"]
  fn get_stride_width(n: Node) -> Width;

  #[link_name = "verilog__nodes__set_stride_width"]
  fn set_stride_width(n: Node, v: Width);

  #[link_name = "verilog__nodes__get_stride_size"]
  fn get_stride_size(n: Node) -> Tsize;

  #[link_name = "verilog__nodes__set_stride_size"]
  fn set_stride_size(n: Node, v: Tsize);

  #[link_name = "verilog__nodes__get_type_hash"]
  fn get_type_hash(n: Node) -> u32;

  #[link_name = "verilog__nodes__set_type_hash"]
  fn set_type_hash(n: Node, v: u32);

  #[link_name = "verilog__nodes__get_maximum_size_expr"]
  fn get_maximum_size_expr(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_maximum_size_expr"]
  fn set_maximum_size_expr(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_maximum_size_cst"]
  fn get_maximum_size_cst(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_maximum_size_cst"]
  fn set_maximum_size_cst(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_lvalue"]
  fn get_lvalue(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_lvalue"]
  fn set_lvalue(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_name"]
  fn get_name(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_name"]
  fn set_name(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_item_name"]
  fn get_item_name(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_item_name"]
  fn set_item_name(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_pattern_key"]
  fn get_pattern_key(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_pattern_key"]
  fn set_pattern_key(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_left"]
  fn get_left(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_left"]
  fn set_left(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_right"]
  fn get_right(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_right"]
  fn set_right(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_repeat_expression"]
  fn get_repeat_expression(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_repeat_expression"]
  fn set_repeat_expression(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_op_attributes"]
  fn get_op_attributes(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_op_attributes"]
  fn set_op_attributes(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_attributes_chain"]
  fn get_attributes_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_attributes_chain"]
  fn set_attributes_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_condition"]
  fn get_condition(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_condition"]
  fn set_condition(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_cond_true"]
  fn get_cond_true(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_cond_true"]
  fn set_cond_true(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_cond_false"]
  fn get_cond_false(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_cond_false"]
  fn set_cond_false(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_true_stmt"]
  fn get_true_stmt(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_true_stmt"]
  fn set_true_stmt(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_false_stmt"]
  fn get_false_stmt(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_false_stmt"]
  fn set_false_stmt(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_pass_stmt"]
  fn get_pass_stmt(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_pass_stmt"]
  fn set_pass_stmt(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_else_stmt"]
  fn get_else_stmt(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_else_stmt"]
  fn set_else_stmt(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_clocking_event"]
  fn get_clocking_event(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_clocking_event"]
  fn set_clocking_event(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_disable_expression"]
  fn get_disable_expression(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_disable_expression"]
  fn set_disable_expression(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_property_expression"]
  fn get_property_expression(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_property_expression"]
  fn set_property_expression(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_true_block"]
  fn get_true_block(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_true_block"]
  fn set_true_block(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_false_block"]
  fn get_false_block(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_false_block"]
  fn set_false_block(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_statement"]
  fn get_statement(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_statement"]
  fn set_statement(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_foreach_array"]
  fn get_foreach_array(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_foreach_array"]
  fn set_foreach_array(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_foreach_variables"]
  fn get_foreach_variables(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_foreach_variables"]
  fn set_foreach_variables(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_control"]
  fn get_control(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_control"]
  fn set_control(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_replication"]
  fn get_replication(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_replication"]
  fn set_replication(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_replication_cst"]
  fn get_replication_cst(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_replication_cst"]
  fn set_replication_cst(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_expressions"]
  fn get_expressions(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_expressions"]
  fn set_expressions(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_elements"]
  fn get_elements(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_elements"]
  fn set_elements(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_slice_size_expr"]
  fn get_slice_size_expr(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_slice_size_expr"]
  fn set_slice_size_expr(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_slice_size_type"]
  fn get_slice_size_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_slice_size_type"]
  fn set_slice_size_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_members"]
  fn get_members(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_members"]
  fn set_members(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_nbr_members"]
  fn get_nbr_members(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_nbr_members"]
  fn set_nbr_members(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_member_index"]
  fn get_member_index(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_member_index"]
  fn set_member_index(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_packed_member_offset"]
  fn get_packed_member_offset(n: Node) -> u32;

  #[link_name = "verilog__nodes__set_packed_member_offset"]
  fn set_packed_member_offset(n: Node, v: u32);

  #[link_name = "verilog__nodes__get_nature_items"]
  fn get_nature_items(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_nature_items"]
  fn set_nature_items(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_discipline_items"]
  fn get_discipline_items(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_discipline_items"]
  fn set_discipline_items(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_continuous_flag"]
  fn get_continuous_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_continuous_flag"]
  fn set_continuous_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_potential_flag"]
  fn get_potential_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_potential_flag"]
  fn set_potential_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_nature"]
  fn get_nature(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_nature"]
  fn set_nature(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_connections"]
  fn get_connections(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_connections"]
  fn set_connections(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_gate_terminals"]
  fn get_gate_terminals(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_gate_terminals"]
  fn set_gate_terminals(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_parameter_values"]
  fn get_parameter_values(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_parameter_values"]
  fn set_parameter_values(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_case_items"]
  fn get_case_items(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_case_items"]
  fn set_case_items(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay"]
  fn get_delay(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay"]
  fn set_delay(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_net_delay"]
  fn get_net_delay(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_net_delay"]
  fn set_net_delay(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_gate_delay"]
  fn get_gate_delay(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_gate_delay"]
  fn set_gate_delay(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_assign_delay"]
  fn get_assign_delay(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_assign_delay"]
  fn set_assign_delay(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_rising_delay"]
  fn get_rising_delay(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_rising_delay"]
  fn set_rising_delay(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_falling_delay"]
  fn get_falling_delay(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_falling_delay"]
  fn set_falling_delay(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_highz_delay"]
  fn get_highz_delay(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_highz_delay"]
  fn set_highz_delay(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_for_initialization"]
  fn get_for_initialization(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_for_initialization"]
  fn set_for_initialization(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_step_assign"]
  fn get_step_assign(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_step_assign"]
  fn set_step_assign(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_arguments"]
  fn get_arguments(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_arguments"]
  fn set_arguments(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_iterator_argument"]
  fn get_iterator_argument(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_iterator_argument"]
  fn set_iterator_argument(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_task"]
  fn get_task(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_task"]
  fn set_task(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_signed_flag"]
  fn get_signed_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_signed_flag"]
  fn set_signed_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_scope_flag"]
  fn get_scope_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_scope_flag"]
  fn set_scope_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_number_base"]
  fn get_number_base(n: Node) -> Base;

  #[link_name = "verilog__nodes__set_number_base"]
  fn set_number_base(n: Node, v: Base);

  #[link_name = "verilog__nodes__get_number_hi_val"]
  fn get_number_hi_val(n: Node) -> u32;

  #[link_name = "verilog__nodes__set_number_hi_val"]
  fn set_number_hi_val(n: Node, v: u32);

  #[link_name = "verilog__nodes__get_number_lo_val"]
  fn get_number_lo_val(n: Node) -> u32;

  #[link_name = "verilog__nodes__set_number_lo_val"]
  fn set_number_lo_val(n: Node, v: u32);

  #[link_name = "verilog__nodes__get_number_hi_zx"]
  fn get_number_hi_zx(n: Node) -> u32;

  #[link_name = "verilog__nodes__set_number_hi_zx"]
  fn set_number_hi_zx(n: Node, v: u32);

  #[link_name = "verilog__nodes__get_number_lo_zx"]
  fn get_number_lo_zx(n: Node) -> u32;

  #[link_name = "verilog__nodes__set_number_lo_zx"]
  fn set_number_lo_zx(n: Node, v: u32);

  #[link_name = "verilog__nodes__get_number_size"]
  fn get_number_size(n: Node) -> Width;

  #[link_name = "verilog__nodes__set_number_size"]
  fn set_number_size(n: Node, v: Width);

  #[link_name = "verilog__nodes__get_expr_origin"]
  fn get_expr_origin(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_expr_origin"]
  fn set_expr_origin(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_bignum_index"]
  fn get_bignum_index(n: Node) -> BnIndex;

  #[link_name = "verilog__nodes__set_bignum_index"]
  fn set_bignum_index(n: Node, v: BnIndex);

  #[link_name = "verilog__nodes__get_bignum_len"]
  fn get_bignum_len(n: Node) -> u32;

  #[link_name = "verilog__nodes__set_bignum_len"]
  fn set_bignum_len(n: Node, v: u32);

  #[link_name = "verilog__nodes__get_real_number"]
  fn get_real_number(n: Node) -> f64;

  #[link_name = "verilog__nodes__set_real_number"]
  fn set_real_number(n: Node, v: f64);

  #[link_name = "verilog__nodes__get_time_unit"]
  fn get_time_unit(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_time_unit"]
  fn set_time_unit(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_scale_factor"]
  fn get_scale_factor(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_scale_factor"]
  fn set_scale_factor(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_time_precision"]
  fn get_time_precision(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_time_precision"]
  fn set_time_precision(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_timescale"]
  fn get_timescale(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_timescale"]
  fn set_timescale(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_string_size"]
  fn get_string_size(n: Node) -> u32;

  #[link_name = "verilog__nodes__set_string_size"]
  fn set_string_size(n: Node, v: u32);

  #[link_name = "verilog__nodes__get_data_type"]
  fn get_data_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_data_type"]
  fn set_data_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_expr_type"]
  fn get_expr_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_expr_type"]
  fn set_expr_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_param_type"]
  fn get_param_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_param_type"]
  fn set_param_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_element_data_type"]
  fn get_element_data_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_element_data_type"]
  fn set_element_data_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_type_element_type"]
  fn get_type_element_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_type_element_type"]
  fn set_type_element_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_cast_data_type"]
  fn get_cast_data_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_cast_data_type"]
  fn set_cast_data_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_base_class_type"]
  fn get_base_class_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_base_class_type"]
  fn set_base_class_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_class_constructor"]
  fn get_class_constructor(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_class_constructor"]
  fn set_class_constructor(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_inheritance_depth"]
  fn get_inheritance_depth(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_inheritance_depth"]
  fn set_inheritance_depth(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_enum_base_data_type"]
  fn get_enum_base_data_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_enum_base_data_type"]
  fn set_enum_base_data_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_enum_base_type"]
  fn get_enum_base_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_enum_base_type"]
  fn set_enum_base_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_packed_base_type"]
  fn get_packed_base_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_packed_base_type"]
  fn set_packed_base_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_default_type"]
  fn get_default_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_default_type"]
  fn set_default_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_type_owner"]
  fn get_type_owner(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_type_owner"]
  fn set_type_owner(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_type_owner_2"]
  fn get_type_owner_2(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_type_owner_2"]
  fn set_type_owner_2(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_forward_type"]
  fn get_forward_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_forward_type"]
  fn set_forward_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_enum_names"]
  fn get_enum_names(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_enum_names"]
  fn set_enum_names(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_index_data_type"]
  fn get_index_data_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_index_data_type"]
  fn set_index_data_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_type_index_type"]
  fn get_type_index_type(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_type_index_type"]
  fn set_type_index_type(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_type_argument"]
  fn get_type_argument(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_type_argument"]
  fn set_type_argument(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_type_signed"]
  fn get_type_signed(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_type_signed"]
  fn set_type_signed(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_subroutine"]
  fn get_subroutine(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_subroutine"]
  fn set_subroutine(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_object"]
  fn get_object(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_object"]
  fn set_object(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_with_expression"]
  fn get_with_expression(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_with_expression"]
  fn set_with_expression(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_drive_strength"]
  fn get_drive_strength(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_drive_strength"]
  fn set_drive_strength(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_net_drive_strength"]
  fn get_net_drive_strength(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_net_drive_strength"]
  fn set_net_drive_strength(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_charge_strength"]
  fn get_charge_strength(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_charge_strength"]
  fn set_charge_strength(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_module"]
  fn get_module(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_module"]
  fn set_module(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_class_name"]
  fn get_class_name(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_class_name"]
  fn set_class_name(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_interface"]
  fn get_interface(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_interface"]
  fn set_interface(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_interface_name"]
  fn get_interface_name(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_interface_name"]
  fn set_interface_name(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_instance"]
  fn get_instance(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_instance"]
  fn set_instance(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_instance_ref"]
  fn get_instance_ref(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_instance_ref"]
  fn set_instance_ref(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_port"]
  fn get_port(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_port"]
  fn set_port(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_collapse_flag"]
  fn get_collapse_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_collapse_flag"]
  fn set_collapse_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_unary_op"]
  fn get_unary_op(n: Node) -> UnaryOps;

  #[link_name = "verilog__nodes__set_unary_op"]
  fn set_unary_op(n: Node, v: UnaryOps);

  #[link_name = "verilog__nodes__get_binary_op"]
  fn get_binary_op(n: Node) -> BinaryOps;

  #[link_name = "verilog__nodes__set_binary_op"]
  fn set_binary_op(n: Node, v: BinaryOps);

  #[link_name = "verilog__nodes__get_conversion_op"]
  fn get_conversion_op(n: Node) -> ConvOps;

  #[link_name = "verilog__nodes__set_conversion_op"]
  fn set_conversion_op(n: Node, v: ConvOps);

  #[link_name = "verilog__nodes__get_declaration"]
  fn get_declaration(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_declaration"]
  fn set_declaration(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_redeclaration"]
  fn get_redeclaration(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_redeclaration"]
  fn set_redeclaration(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_this_declaration"]
  fn get_this_declaration(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_this_declaration"]
  fn set_this_declaration(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_default_value"]
  fn get_default_value(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_default_value"]
  fn set_default_value(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_instantiated_flag"]
  fn get_instantiated_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_instantiated_flag"]
  fn set_instantiated_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_ansi_port_flag"]
  fn get_ansi_port_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_ansi_port_flag"]
  fn set_ansi_port_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_event"]
  fn get_event(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_event"]
  fn set_event(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_min_expr"]
  fn get_min_expr(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_min_expr"]
  fn set_min_expr(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_typ_expr"]
  fn get_typ_expr(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_typ_expr"]
  fn set_typ_expr(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_max_expr"]
  fn get_max_expr(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_max_expr"]
  fn set_max_expr(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_udp_port_declaration_chain"]
  fn get_udp_port_declaration_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_udp_port_declaration_chain"]
  fn set_udp_port_declaration_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_udp_kind"]
  fn get_udp_kind(n: Node) -> UdpKind;

  #[link_name = "verilog__nodes__set_udp_kind"]
  fn set_udp_kind(n: Node, v: UdpKind);

  #[link_name = "verilog__nodes__get_udp_initial"]
  fn get_udp_initial(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_udp_initial"]
  fn set_udp_initial(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_udp_entries_chain"]
  fn get_udp_entries_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_udp_entries_chain"]
  fn set_udp_entries_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_input_chain"]
  fn get_input_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_input_chain"]
  fn set_input_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_output_symbol"]
  fn get_output_symbol(n: Node) -> UdpSymbol;

  #[link_name = "verilog__nodes__set_output_symbol"]
  fn set_output_symbol(n: Node, v: UdpSymbol);

  #[link_name = "verilog__nodes__get_current_state"]
  fn get_current_state(n: Node) -> UdpSymbol;

  #[link_name = "verilog__nodes__set_current_state"]
  fn set_current_state(n: Node, v: UdpSymbol);

  #[link_name = "verilog__nodes__get_next_state"]
  fn get_next_state(n: Node) -> UdpSymbol;

  #[link_name = "verilog__nodes__set_next_state"]
  fn set_next_state(n: Node, v: UdpSymbol);

  #[link_name = "verilog__nodes__get_symbol"]
  fn get_symbol(n: Node) -> UdpSymbol;

  #[link_name = "verilog__nodes__set_symbol"]
  fn set_symbol(n: Node, v: UdpSymbol);

  #[link_name = "verilog__nodes__get_from_symbol"]
  fn get_from_symbol(n: Node) -> UdpSymbol;

  #[link_name = "verilog__nodes__set_from_symbol"]
  fn set_from_symbol(n: Node, v: UdpSymbol);

  #[link_name = "verilog__nodes__get_to_symbol"]
  fn get_to_symbol(n: Node) -> UdpSymbol;

  #[link_name = "verilog__nodes__set_to_symbol"]
  fn set_to_symbol(n: Node, v: UdpSymbol);

  #[link_name = "verilog__nodes__get_specify_input"]
  fn get_specify_input(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_specify_input"]
  fn set_specify_input(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_specify_output"]
  fn get_specify_output(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_specify_output"]
  fn set_specify_output(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_path_delay"]
  fn get_path_delay(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_path_delay"]
  fn set_path_delay(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_data_source"]
  fn get_data_source(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_data_source"]
  fn set_data_source(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_polarity"]
  fn get_polarity(n: Node) -> Polarity;

  #[link_name = "verilog__nodes__set_polarity"]
  fn set_polarity(n: Node, v: Polarity);

  #[link_name = "verilog__nodes__get_delay_rise"]
  fn get_delay_rise(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_rise"]
  fn set_delay_rise(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_fall"]
  fn get_delay_fall(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_fall"]
  fn set_delay_fall(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_z"]
  fn get_delay_z(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_z"]
  fn set_delay_z(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_01"]
  fn get_delay_01(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_01"]
  fn set_delay_01(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_10"]
  fn get_delay_10(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_10"]
  fn set_delay_10(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_0z"]
  fn get_delay_0z(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_0z"]
  fn set_delay_0z(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_z1"]
  fn get_delay_z1(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_z1"]
  fn set_delay_z1(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_1z"]
  fn get_delay_1z(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_1z"]
  fn set_delay_1z(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_z0"]
  fn get_delay_z0(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_z0"]
  fn set_delay_z0(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_0x"]
  fn get_delay_0x(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_0x"]
  fn set_delay_0x(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_x1"]
  fn get_delay_x1(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_x1"]
  fn set_delay_x1(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_1x"]
  fn get_delay_1x(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_1x"]
  fn set_delay_1x(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_x0"]
  fn get_delay_x0(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_x0"]
  fn set_delay_x0(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_xz"]
  fn get_delay_xz(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_xz"]
  fn set_delay_xz(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_zx"]
  fn get_delay_zx(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_zx"]
  fn set_delay_zx(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_string_id"]
  fn get_string_id(n: Node) -> String8Id;

  #[link_name = "verilog__nodes__set_string_id"]
  fn set_string_id(n: Node, v: String8Id);

  #[link_name = "verilog__nodes__get_label"]
  fn get_label(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_label"]
  fn set_label(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_label_number"]
  fn get_label_number(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_label_number"]
  fn set_label_number(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_label_chain"]
  fn get_label_chain(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_label_chain"]
  fn set_label_chain(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_label_use"]
  fn get_label_use(n: Node) -> i32;

  #[link_name = "verilog__nodes__set_label_use"]
  fn set_label_use(n: Node, v: i32);

  #[link_name = "verilog__nodes__get_suspend_flag"]
  fn get_suspend_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_suspend_flag"]
  fn set_suspend_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_same_case_flag"]
  fn get_same_case_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_same_case_flag"]
  fn set_same_case_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_obj_id"]
  fn get_obj_id(n: Node) -> ObjId;

  #[link_name = "verilog__nodes__set_obj_id"]
  fn set_obj_id(n: Node, v: ObjId);

  #[link_name = "verilog__nodes__get_scope_id"]
  fn get_scope_id(n: Node) -> ScopeId;

  #[link_name = "verilog__nodes__set_scope_id"]
  fn set_scope_id(n: Node, v: ScopeId);

  #[link_name = "verilog__nodes__get_process_id"]
  fn get_process_id(n: Node) -> ProcId;

  #[link_name = "verilog__nodes__set_process_id"]
  fn set_process_id(n: Node, v: ProcId);

  #[link_name = "verilog__nodes__get_sys_tf_id"]
  fn get_sys_tf_id(n: Node) -> SysTfId;

  #[link_name = "verilog__nodes__set_sys_tf_id"]
  fn set_sys_tf_id(n: Node, v: SysTfId);

  #[link_name = "verilog__nodes__get_lit_id"]
  fn get_lit_id(n: Node) -> LitId;

  #[link_name = "verilog__nodes__set_lit_id"]
  fn set_lit_id(n: Node, v: LitId);

  #[link_name = "verilog__nodes__get_generate_block"]
  fn get_generate_block(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_generate_block"]
  fn set_generate_block(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_input_skew"]
  fn get_input_skew(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_input_skew"]
  fn set_input_skew(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_output_skew"]
  fn get_output_skew(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_output_skew"]
  fn set_output_skew(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_delay_control"]
  fn get_delay_control(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_delay_control"]
  fn set_delay_control(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_attribute_item"]
  fn get_attribute_item(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_attribute_item"]
  fn set_attribute_item(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_has_identifier_list"]
  fn get_has_identifier_list(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_identifier_list"]
  fn set_has_identifier_list(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_has_sign"]
  fn get_has_sign(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_sign"]
  fn set_has_sign(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_connected_flag"]
  fn get_connected_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_connected_flag"]
  fn set_connected_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_complete_flag"]
  fn get_complete_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_complete_flag"]
  fn set_complete_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_implicit_flag"]
  fn get_implicit_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_implicit_flag"]
  fn set_implicit_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_redeclaration_flag"]
  fn get_redeclaration_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_redeclaration_flag"]
  fn set_redeclaration_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_is_automatic"]
  fn get_is_automatic(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_is_automatic"]
  fn set_is_automatic(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_lifetime"]
  fn get_lifetime(n: Node) -> Lifetime;

  #[link_name = "verilog__nodes__set_lifetime"]
  fn set_lifetime(n: Node, v: Lifetime);

  #[link_name = "verilog__nodes__get_has_lifetime"]
  fn get_has_lifetime(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_lifetime"]
  fn set_has_lifetime(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_has_end_name"]
  fn get_has_end_name(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_end_name"]
  fn set_has_end_name(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_call"]
  fn get_call(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_call"]
  fn set_call(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_timeunit"]
  fn get_timeunit(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_timeunit"]
  fn set_timeunit(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_timeprecision"]
  fn get_timeprecision(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_timeprecision"]
  fn set_timeprecision(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_error_origin"]
  fn get_error_origin(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_error_origin"]
  fn set_error_origin(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_has_void_cast"]
  fn get_has_void_cast(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_void_cast"]
  fn set_has_void_cast(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_is_const"]
  fn get_is_const(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_is_const"]
  fn set_is_const(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_has_var"]
  fn get_has_var(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_var"]
  fn set_has_var(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_has_type"]
  fn get_has_type(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_type"]
  fn set_has_type(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_has_direction"]
  fn get_has_direction(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_direction"]
  fn set_has_direction(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_has_parenthesis"]
  fn get_has_parenthesis(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_parenthesis"]
  fn set_has_parenthesis(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_has_argument"]
  fn get_has_argument(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_argument"]
  fn set_has_argument(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_fully_analyzed_flag"]
  fn get_fully_analyzed_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_fully_analyzed_flag"]
  fn set_fully_analyzed_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_resolved_flag"]
  fn get_resolved_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_resolved_flag"]
  fn set_resolved_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_mark_flag"]
  fn get_mark_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_mark_flag"]
  fn set_mark_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_is_constant"]
  fn get_is_constant(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_is_constant"]
  fn set_is_constant(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_static_flag"]
  fn get_static_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_static_flag"]
  fn set_static_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_has_attribute"]
  fn get_has_attribute(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_attribute"]
  fn set_has_attribute(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_attribute_full"]
  fn get_attribute_full(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_attribute_full"]
  fn set_attribute_full(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_attribute_parallel"]
  fn get_attribute_parallel(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_attribute_parallel"]
  fn set_attribute_parallel(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_other_attributes"]
  fn get_other_attributes(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_other_attributes"]
  fn set_other_attributes(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_pure_property"]
  fn get_pure_property(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_pure_property"]
  fn set_pure_property(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_context_property"]
  fn get_context_property(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_context_property"]
  fn set_context_property(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_has_extern_flag"]
  fn get_has_extern_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_extern_flag"]
  fn set_has_extern_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_virtual_flag"]
  fn get_virtual_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_virtual_flag"]
  fn set_virtual_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_pure_flag"]
  fn get_pure_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_pure_flag"]
  fn set_pure_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_join_option"]
  fn get_join_option(n: Node) -> Join;

  #[link_name = "verilog__nodes__set_join_option"]
  fn set_join_option(n: Node, v: Join);

  #[link_name = "verilog__nodes__get_edge_identifier"]
  fn get_edge_identifier(n: Node) -> Edge;

  #[link_name = "verilog__nodes__set_edge_identifier"]
  fn set_edge_identifier(n: Node, v: Edge);

  #[link_name = "verilog__nodes__get_dpi_spec"]
  fn get_dpi_spec(n: Node) -> DPISpec;

  #[link_name = "verilog__nodes__set_dpi_spec"]
  fn set_dpi_spec(n: Node, v: DPISpec);

  #[link_name = "verilog__nodes__get_visibility"]
  fn get_visibility(n: Node) -> Visibility;

  #[link_name = "verilog__nodes__set_visibility"]
  fn set_visibility(n: Node, v: Visibility);

  #[link_name = "verilog__nodes__get_class_visibility"]
  fn get_class_visibility(n: Node) -> Visibility;

  #[link_name = "verilog__nodes__set_class_visibility"]
  fn set_class_visibility(n: Node, v: Visibility);

  #[link_name = "verilog__nodes__get_has_visibility"]
  fn get_has_visibility(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_has_visibility"]
  fn set_has_visibility(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_violation"]
  fn get_violation(n: Node) -> Violation;

  #[link_name = "verilog__nodes__set_violation"]
  fn set_violation(n: Node, v: Violation);

  #[link_name = "verilog__nodes__get_random_flag"]
  fn get_random_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_random_flag"]
  fn set_random_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_randc_flag"]
  fn get_randc_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_randc_flag"]
  fn set_randc_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_size_flag"]
  fn get_size_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_size_flag"]
  fn set_size_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_type_analyzed_flag"]
  fn get_type_analyzed_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_type_analyzed_flag"]
  fn set_type_analyzed_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_forward_typedef_flag"]
  fn get_forward_typedef_flag(n: Node) -> bool;

  #[link_name = "verilog__nodes__set_forward_typedef_flag"]
  fn set_forward_typedef_flag(n: Node, v: bool);

  #[link_name = "verilog__nodes__get_access"]
  fn get_access(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_access"]
  fn set_access(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_arg1"]
  fn get_arg1(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_arg1"]
  fn set_arg1(n: Node, v: Node);

  #[link_name = "verilog__nodes__get_arg2"]
  fn get_arg2(n: Node) -> Node;

  #[link_name = "verilog__nodes__set_arg2"]
  fn set_arg2(n: Node, v: Node);

}
