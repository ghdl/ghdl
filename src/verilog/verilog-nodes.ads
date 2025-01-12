--  Verilog AST nodes
--  Copyright (C) 2023 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.
with Types; use Types;
with Verilog.Types; use Verilog.Types;

package Verilog.Nodes is
   type Nkind is
     (
      N_Error,

      N_Error_Expr,

      N_Timescale_Directive,
      N_Timeunits_Declaration,
      N_Timeunit,
      N_Timeprecision,

      --  Scalar data types.
      N_Logic_Type,
      N_Bit_Type,
      N_Real_Type,
      N_Shortreal_Type,

      --  Aggregate data types.
      N_Log_Packed_Array_Cst,
      N_Bit_Packed_Array_Cst,
      N_Array_Cst,
      N_Packed_Array,
      N_Array,
      N_Struct_Type,
      N_Packed_Struct_Type,
      N_Union_Type,
      N_Packed_Union_Type,
      N_Queue,
      N_Queue_Cst,
      N_Dynamic_Array_Cst,
      N_Dynamic_Array,
      N_Associative_Array,
      N_Associative_Array_Cst,

      --  Other types
      N_Enum_Type,
      N_String_Type,
      N_Chandle_Type,
      N_Event_Type,
      N_Virtual_Interface,
      N_Void_Type,
      N_Error_Type,
      N_Null_Type,   --  Type of null.
      N_Nature,
      N_Class,
      N_Instantiated_Class,  --  Class with parameters
      N_Class_Instance,

      N_Generic_Class,

      N_Wildcard_Type,

      --  Hierarchy.
      N_Compilation_Unit,
      N_Foreign_Module,
      N_Module,
      N_Primitive,
      N_Interface_Declaration,
      N_Package,
      N_Program_Declaration,
      N_Port,
      N_Task,
      N_Function,
      N_OOB_Task,
      N_OOB_Function,
      N_Extern_Task,
      N_Extern_Function,
      N_Import_DPI_Function,
      N_Export_DPI_Function,
      N_Export_DPI_Task,
      N_Clocking,
      N_Default_Clocking,
      N_Disable_Iff,
      N_Specify,
      N_Property_Declaration,

      N_Input,          --  Nkinds_Net_Port
      N_Inout,          --  Nkinds_Net_Port
      N_Output,         --  Nkinds_Net_Port
      N_Interface_Port,
      N_Modport_Port,
      N_Tf_Input,       --  Nkinds_Tf_Port
      N_Tf_Inout,       --  Nkinds_Tf_Port
      N_Tf_Output,      --  Nkinds_Tf_Port
      N_Tf_Ref,         --  Nkinds_Tf_Port
      N_Tf_Const_Ref,   --  Nkinds_Tf_Port

      --  Declarations
      N_Parameter,
      N_Type_Parameter,
      N_Localparam,
      N_Type_Localparam,
      N_Var,
      N_Return_Var,     --  The implicit return variable of a function.
      N_This_Var,       --  The implicit variable for 'this' (in methods).
      N_Iterator_Argument,
      N_Wire_Direct,
      N_Wire,
      N_Tri,
      N_Wand,
      N_Triand,
      N_Wor,
      N_Trior,
      N_Tri0,
      N_Tri1,
      N_Supply0,
      N_Supply1,
      N_Uwire,
      N_Trireg,
      N_Typedef,
      N_Typedef_Class,
      N_Typedef_Struct,
      N_Typedef_Forward,
      N_Predefined_Typedef,
      N_Package_Import,
      N_Genvar,
      N_Enum_Name,
      N_Enum_Range,
      N_Foreach_Variable,
      N_Clock_Var,
      N_Modport,
      N_Modport_Input,
      N_Modport_Output,
      N_Modport_Inout,
      N_Modport_Ref,
      N_Modport_Clocking,
      N_Modport_Import_Tf,
      N_Modport_Export_Tf,
      N_Constraint,
      N_Constraint_Expression,
      N_Constraint_If,
      N_Constraint_Foreach,

      --  Verilog AMS declarations
      N_Discipline,
      N_Branch,
      N_Port_Branch,

      N_Nature_Attribute,
      N_Nature_Access,
      N_Discipline_Domain,
      N_Discipline_Potential,
      N_Discipline_Flow,
      N_Discipline_Attribute,

      N_From_Range,
      N_Exclude_Range,

      --  Module statements.
      N_Assign,
      N_Decl_Assign,  --  Implicit assign process for a net declaration
      N_Always,
      N_Always_Comb,
      N_Always_Latch,
      N_Always_Ff,
      N_Initial,
      N_Final,
      N_Debug,
      N_Module_Instance,
      N_Primitive_Instance,
      N_Interface_Instance,
      N_Program_Instance,
      N_Parameter_Value_Type,
      N_Parameter_Value_Expr,
      N_Defparam,
      N_Generate_Region,
      N_Loop_Generate,
      N_If_Generate,
      N_Case_Generate,
      N_Generate_Block,
      N_Array_Generate_Block,
      N_Indexed_Generate_Block,
      N_Analog,
      N_Assert_Property,
      N_Assume_Property,

      N_Gate_And,
      N_Gate_Nand,
      N_Gate_Or,
      N_Gate_Nor,
      N_Gate_Xor,
      N_Gate_Xnor,

      N_Gate_Buf,
      N_Gate_Not,

      N_Gate_Bufif0,
      N_Gate_Bufif1,
      N_Gate_Notif0,
      N_Gate_Notif1,

      N_Gate_Nmos,
      N_Gate_Pmos,
      N_Gate_Rnmos,
      N_Gate_Rpmos,

      N_Gate_Tran,
      N_Gate_Rtran,

      N_Gate_Tranif0,
      N_Gate_Tranif1,
      N_Gate_Rtranif0,
      N_Gate_Rtranif1,

      N_Gate_Cmos,
      N_Gate_Rcmos,

      N_Gate_Pullup,
      N_Gate_Pulldown,

      N_Default_Skew,
      N_Clocking_Skew,

      --  Terminal of gate instantiation.
      N_Control_Terminal,
      N_Input_Terminal,
      N_Inout_Terminal,
      N_Output_Terminal,

      N_Port_Connection,
      N_Wildcard_Connection,
      N_Implicit_Connection,
      N_Default_Connection,

      --  Procedural statements.
      N_Seq_Block,
      N_Par_Block,
      N_If,
      N_For,
      N_While,
      N_Do_While,
      N_Foreach,
      N_Repeat,
      N_Forever,
      N_Wait,
      N_Wait_Fork,
      N_Trigger,
      N_Disable,
      N_Disable_Fork,
      N_Proc_Assign,
      N_Proc_Deassign,
      N_Noblk_Assign,       --  <=
      N_Blocking_Assign,    --  =
      N_Unpack_Assign,
      N_Pack_Assign,
      N_Pack_Unpack_Assign,
      N_Assign_Operator,
      N_Force_Assign,
      N_Release,
      N_Case,
      N_Casex,
      N_Casez,
      N_Case_Item,
      N_Default_Case_Item,
      N_Subroutine_Call_Stmt,
      N_Return_Stmt,
      N_Break_Stmt,
      N_Continue_Stmt,
      N_Label_Stmt,
      N_Simple_Immediate_Assert,
      N_Argument,

      --  Analog statements
      N_Contribution,       -- <+

      --  Expressions
      N_Name,
      N_This_Name,
      N_Dotted_Name,
      N_Scoped_Name,
      N_Interface_Item,
      N_Modport_Item,
      N_Wildcard_Name,
      N_Property_Name,
      N_Class_Qualified_Name,
      N_Method_Name,
      N_Member_Name,
      N_Hierarchical,
      N_Number,
      N_Computed_Number,
      N_Bignum,
      N_Unbased_Literal,
      N_Time_Literal,
      N_1step_Literal,
      N_Infinity,
      N_Real_Number,
      N_Scale_Number,
      N_Mintypmax,
      N_Bit_Select,
      N_Part_Select,
      N_Plus_Part_Select,   --  +:
      N_Minus_Part_Select,  --  -:
      N_Indexed_Name,       --  [] for arrays
      N_String_Index,       --  [] for a string
      N_Associative_Index,  --  [] for an associative array
      N_Slice_Name,
      N_Part_Select_Cst,
      N_Plus_Part_Select_Cst,
      N_Minus_Part_Select_Cst,
      N_Slice_Name_Cst,
      N_Member_Select,
      N_String_Literal,
      N_Implicit_Event,     --  @*
      N_New_Call,
      N_New_Expression,
      N_Dynamic_Array_New,
      N_Parenthesis_Expr,
      N_Type_Cast,
      N_Size_Cast,
      N_Null,
      N_This,
      N_Super,
      N_Default,
      N_Aggregate_Literal,
      N_Aggregate_Literal_Cst,
      N_Aggregate_Element,

      N_Event_Control,
      N_Delay_Control,
      N_Repeat_Control,
      N_Cycle_Delay,
      N_Posedge,
      N_Negedge,
      N_Or,
      N_Delay,
      N_Element,
      N_Value_Range,
      N_Stream_Expression,

      --  Operations.
      N_Left_Streaming_Expr,
      N_Right_Streaming_Expr,
      N_Left_Streaming_Type,
      N_Right_Streaming_Type,
      N_Concatenation,
      N_Membership,
      N_Replication_Cst,
      N_Cond_Op,
      N_Call,
      N_Array_Method_Call,
      N_Randomize_Call,
      N_System_Call,
      N_Bits_Expr,
      N_Bits_Type,
      N_Binary_Op,
      N_Short_Circuit_Op,
      N_Unary_Op,
      N_Post_Increment,
      N_Pre_Increment,
      N_Post_Decrement,
      N_Pre_Decrement,
      N_Access_Call,

      --  Conversions.
      N_Conversion,

      --  Sequences
      N_Seq_Repeat,        --  [*  ]
      N_Seq_Plus_Repeat,   --  [+]
      N_Seq_Star_Repeat,   --  [*]
      N_Seq_Star_Concat,   --  ##[*]
      N_Seq_Plus_Concat,   --  ##[+]
      N_Seq_Const_Concat,  --  ## cst
      N_Seq_Range_Concat,  --  ##[ expr ]
      N_Seq_Throughout,    --  throughout
      N_Seq_Parenthesis,

      --  Properties
      N_Prop_Not,
      N_Prop_Or,
      N_Prop_And,
      N_Prop_Overlap_Imp,      --  |->
      N_Prop_Non_Overlap_Imp,  --  |=>
      N_Prop_Until,

      N_Specparam,
      N_Pulse_Control_Specparam,
      N_Ifnone,
      N_Timing_Check,
      N_Par_Path,
      N_Full_Path,
      N_Par_Edge_Path,
      N_Full_Edge_Path,
      N_Path_Element,
      N_Path_Delay3,
      N_Path_Delay6,
      N_Path_Delay12,

      N_Member,
      N_Packed_Member,
      N_Udp_Combinational_Entry,
      N_Udp_Sequential_Entry,
      N_Udp_Level_Symbol,
      N_Udp_Change_Symbol,
      N_Attribute,

      N_Label,
      N_Goto
     );

   subtype Nkinds_Delay_Nets is Nkind range
     N_Wire ..
   --N_Tri
   --N_Wand
   --N_Triand
   --N_Wor
   --N_Trior
   --N_Tri0
   --N_Tri1
   --N_Supply0
   --N_Supply1
   --N_Uwire
     N_Trireg;

   subtype Nkinds_Nets is Nkind range
     N_Wire_Direct ..
   --N_Wire
   --N_Tri
   --N_Wand
   --N_Triand
   --N_Wor
   --N_Trior
   --N_Tri0
   --N_Tri1
   --N_Supply0
   --N_Supply1
   --N_Uwire
     N_Trireg;

   subtype Nkinds_Types is Nkind range
     N_Logic_Type ..
   --N_Bit_Type
   --N_Real_Type
   --N_Shortreal_Type
   --N_Log_Packed_Array_Cst
   --N_Bit_Packed_Array_Cst
   --N_Array_Cst
   --N_Packed_Array
   --N_Array
   --N_Struct_Type
   --N_Packed_Struct_Type
   --N_Union_Type
   --N_Packed_Union_Type
   --N_Queue
   --N_Queue_Cst
   --N_Dynamic_Array_Cst
   --N_Dynamic_Array
   --N_Associative_Array
   --N_Associative_Array_Cst
   --N_Enum_Type
   --N_String_Type
   --N_Chandle_Type
   --N_Event_Type
   --N_Virtual_Interface
   --N_Void_Type
   --N_Error_Type
   --N_Null_Type
   --N_Nature
   --N_Class
   --N_Instantiated_Class
     N_Class_Instance;

   subtype Nkinds_Scalar_Types is Nkind range
     N_Logic_Type ..
   --N_Bit_Type
   --N_Real_Type
     N_Shortreal_Type;

   subtype Nkinds_Vector_Types is Nkind range
     N_Log_Packed_Array_Cst ..
     N_Bit_Packed_Array_Cst;

   subtype Nkinds_Forward_Typedef is Nkind range
     N_Typedef_Class ..
   --N_Typedef_Struct
     N_Typedef_Forward;

   --  Class types, after compilation.
   subtype Nkinds_Class is Nkind range
     N_Class ..
     N_Instantiated_Class;

   --  Any kind of class after parse.
   subtype Nkinds_Any_Class is Nkind range
     N_Class ..
   --N_Instantiated_Class
   --N_Class_Instance
     N_Generic_Class;

   subtype Nkinds_Process is Nkind range
     N_Always ..
   --N_Always_Comb
   --N_Always_Latch
   --N_Always_Ff
   --N_Initial
   --N_Final
     N_Debug;

   subtype Nkinds_Net_Port is Nkind range
     N_Input ..
   --N_Inout
     N_Output;

   subtype Nkinds_Port is Nkind range
     N_Input ..
   --N_Inout
   --N_Output
   --N_Interface_Port
     N_Modport_Port;

   subtype Nkinds_Module is Nkind range
     N_Foreign_Module ..
     N_Module;

   subtype Nkinds_Tf_Port is Nkind range
     N_Tf_Input ..
   --N_Tf_Inout
   --N_Tf_Output
   --N_Tf_Ref
     N_Tf_Const_Ref;

   subtype Nkinds_Tf is Nkind range
     N_Task ..
     N_Function;

   subtype Nkinds_OOB_Tf is Nkind range
     N_OOB_Task ..
     N_OOB_Function;

   subtype Nkinds_Any_Tf is Nkind range
     N_Task ..
   --N_Function
   --N_OOB_Task
   --N_OOB_Function
   --N_Extern_Task
     N_Extern_Function;

   subtype Nkinds_Modport_Port is Nkind range
     N_Modport_Input ..
   --N_Modport_Output
   --N_Modport_Inout
   --N_Modport_Ref
   --N_Modport_Clocking
   --N_Modport_Import_Tf
     N_Modport_Export_Tf;

   subtype Nkinds_Gate is Nkind range
     N_Gate_And ..
   --N_Gate_Nand
   --N_Gate_Or
   --N_Gate_Nor
   --N_Gate_Xor
   --N_Gate_Xnor
   --N_Gate_Buf
   --N_Gate_Not
   --N_Gate_Bufif0
   --N_Gate_Bufif1
   --N_Gate_Notif0
   --N_Gate_Notif1
   --N_Gate_Nmos
   --N_Gate_Pmos
   --N_Gate_Rnmos
   --N_Gate_Rpmos
   --N_Gate_Tran
   --N_Gate_Rtran
   --N_Gate_Tranif0
   --N_Gate_Tranif1
   --N_Gate_Rtranif0
   --N_Gate_Rtranif1
   --N_Gate_Cmos
   --N_Gate_Rcmos
   --N_Gate_Pullup
     N_Gate_Pulldown;

   subtype Nkinds_Input_Gate is Nkind range
     N_Gate_And ..
   --N_Gate_Nand
   --N_Gate_Or
   --N_Gate_Nor
   --N_Gate_Xor
     N_Gate_Xnor;

   subtype Nkinds_Output_Gate is Nkind range
     N_Gate_Buf ..
     N_Gate_Not;

   subtype Nkinds_Enable_Gate is Nkind range
     N_Gate_Bufif0 ..
   --N_Gate_Bufif1
   --N_Gate_Notif0
     N_Gate_Notif1;

   subtype Nkinds_Mos_Switch is Nkind range
     N_Gate_Nmos ..
   --N_Gate_Pmos
   --N_Gate_Rnmos
     N_Gate_Rpmos;

   subtype Nkinds_Pass_Switch is Nkind range
     N_Gate_Tran ..
     N_Gate_Rtran;

   subtype Nkinds_Pass_En_Switch is Nkind range
     N_Gate_Tranif0 ..
   --N_Gate_Tranif1
   --N_Gate_Rtranif0
     N_Gate_Rtranif1;

   subtype Nkinds_Cmos_Switch is Nkind range
     N_Gate_Cmos ..
     N_Gate_Rcmos;

   subtype Nkinds_Pull_Gate is Nkind range
     N_Gate_Pullup ..
     N_Gate_Pulldown;

   subtype Nkinds_Terminal is Nkind range
     N_Control_Terminal ..
   --N_Input_Terminal
   --N_Inout_Terminal
     N_Output_Terminal;

   subtype Nkinds_Connection is Nkind range
     N_Port_Connection ..
   --N_Wildcard_Connection
   --N_Implicit_Connection
     N_Default_Connection;

   subtype Nkinds_Terminal_Or_Connection is Nkind range
     N_Control_Terminal ..
   --N_Input_Terminal
   --N_Inout_Terminal
   --N_Output_Terminal
   --N_Port_Connection
     N_Wildcard_Connection;

   subtype Nkinds_Case is Nkind range
     N_Case ..
   --N_Casex
     N_Casez;

   subtype Nkinds_Case_Item is Nkind range
     N_Case_Item ..
     N_Default_Case_Item;

   subtype Nkinds_Inc_Dec is Nkind range
     N_Post_Increment ..
   --N_Pre_Increment
   --N_Post_Decrement
     N_Pre_Decrement;

   subtype Nkinds_Streaming is Nkind range
     N_Left_Streaming_Expr ..
   --N_Right_Streaming_Expr
   --N_Left_Streaming_Type
     N_Right_Streaming_Type;

   subtype Nkinds_Part_Select is Nkind range
     N_Part_Select ..
   --N_Plus_Part_Select
     N_Minus_Part_Select;

   subtype Nkinds_Indexed_Part_Select is Nkind range
     N_Plus_Part_Select ..
     N_Minus_Part_Select;

   subtype Nkinds_Instance is Nkind range
     N_Module_Instance ..
   --N_Primitive_Instance
   --N_Interface_Instance
     N_Program_Instance;

   subtype Nkinds_Call is Nkind range
     N_Call ..
   --N_Array_Method_Call
     N_Randomize_Call;

   subtype Nkinds_Edge is Nkind range
     N_Posedge ..
     N_Negedge;

   subtype Nkinds_Seq_Expr is Nkind range
     N_Seq_Repeat ..
   --N_Seq_Plus_Repeat
   --N_Seq_Star_Repeat
   --N_Seq_Star_Concat
   --N_Seq_Plus_Concat
   --N_Seq_Const_Concat
   --N_Seq_Range_Concat
   --N_Seq_Throughout
     N_Seq_Parenthesis;

   subtype Nkinds_Prop_Expr is Nkind range
     N_Prop_Not ..
   --N_Prop_Or
   --N_Prop_And
   --N_Prop_Overlap_Imp
   --N_Prop_Non_Overlap_Imp
     N_Prop_Until;

   -- subtype Regs_Type_Node is Node range Reg_Type_Node .. Time_Type_Node;

   type Binary_Ops is
     (
      --  Non existing binary operation.  May be used to detected
      --  uninitialized variables.
      Binop_Unknown,

      --  Logical operators
      Binop_Logic_And,      --  &&
      Binop_Logic_Or,       --  ||
      Binop_Logic_Imp,      --  ->
      Binop_Logic_Eqv,      --  <->

      --  Relational operators.
      Binop_Ult,            --  <
      Binop_Slt,            --  <
      Binop_Ule,            --  <=
      Binop_Sle,            --  <=
      Binop_Ugt,            --  >
      Binop_Sgt,            --  >
      Binop_Uge,            --  >=
      Binop_Sge,            --  >=

      --  Equality operators.
      Binop_Log_Eq,         --  ==
      Binop_Log_Ne,         --  !=
      Binop_Case_Eq,        --  ===
      Binop_Case_Ne,        --  !==

      --  Bitwise operators
      Binop_Bit_And,        --  &
      Binop_Bit_Or,         --  |
      Binop_Bit_Xor,        --  ^
      Binop_Bit_Xnor,       --  ^~
      Binop_Bit_Nxor,       --  ~^

      --  Arithmetic operators
      Binop_Add,            --  +
      Binop_Sub,            --  -
      Binop_Umul,           --  * (unsigned)
      Binop_Smul,           --  * (signed)
      Binop_Udiv,           --  / (unsigned)
      Binop_Sdiv,           --  / (signed)
      Binop_Umod,           --  % (unsigned)
      Binop_Smod,           --  % (signed)

      Binop_Exp,             --  **

      Binop_Left_Lshift,     --  <<
      Binop_Right_Lshift,    --  >>
      Binop_Left_Ashift,     --  <<<
      Binop_Right_Ashift     --  >>>
     );

   --  Size handling is the same for relational operators.
   subtype Binary_Short_Circuit_Ops is Binary_Ops
     range Binop_Logic_And .. Binop_Logic_Or;

   subtype Binary_Relational_Ops is Binary_Ops
     range Binop_Ult .. Binop_Sge;

   subtype Binary_Equality_Ops is Binary_Ops
     range Binop_Log_Eq .. Binop_Case_Ne;

   subtype Binary_Logical_Ops is Binary_Ops
     range Binop_Logic_And .. Binop_Logic_Or;

   subtype Binary_Bitwise_Ops is Binary_Ops
     range Binop_Bit_And .. Binop_Bit_Nxor;

   subtype Binary_Arith_Ops is Binary_Ops
     range Binop_Add .. Binop_Smod;

   subtype Binary_Shift_Ops is Binary_Ops
     range Binop_Left_Lshift .. Binop_Right_Ashift;

   type Unary_Ops is
     (
      Unop_Plus,
      Unop_Minus,

      Unop_Bit_Neg,    --  ~
      Unop_Logic_Neg,  --  !

      Unop_Red_Or,
      Unop_Red_Nor,
      Unop_Red_And,
      Unop_Red_Nand,
      Unop_Red_Xor,
      Unop_Red_Xnor,
      Unop_Red_Nxor
     );

   subtype Unary_Red_Ops is Unary_Ops range
     Unop_Red_Or ..
   --Unop_Red_Nor
   --Unop_Red_And
   --Unop_Red_Nand
   --Unop_Red_Xor
   --Unop_Red_Xnor
     Unop_Red_Nxor;

   type Conv_Ops is
     (
      --  No conversion.
      Convop_None,

      --  Logic vectors
      Convop_Lv_Zext,
      Convop_Lv_Sext,
      Convop_Lv_Trunc,
      Convop_Lv_Nop,    --  Sign

      Convop_Lv_Log,    --  LV to logic
      Convop_Lv_Bit,    --  LV to bit

      --  Logic vector to bit vector.
      Convop_Lv_Bv_Zext,
      Convop_Lv_Bv_Sext,
      Convop_Lv_Bv_Trunc,
      Convop_Lv_Bv,

      Convop_Lv_Float,

      --  Bit vectors
      Convop_Bv_Zext,
      Convop_Bv_Sext,
      Convop_Bv_Trunc,
      Convop_Bv_Nop,    --  Sign

      Convop_Bv_Log,    --  BV to logic
      Convop_Bv_Bit,    --  BV to bit

      --  Bit vector to logic vector.
      Convop_Bv_Lv_Zext,
      Convop_Bv_Lv_Sext,
      Convop_Bv_Lv_Trunc,
      Convop_Bv_Lv,

      Convop_Bv_Float,

      --  Logical
      Convop_Log_Bit,

      Convop_Log_Slv,   --  Logic to signed LV
      Convop_Log_Ulv,

      Convop_Log_Sbv,   --  Logic to signed BV
      Convop_Log_Ubv,

      Convop_Log_Real,
      Convop_Log_Shortreal,

      --  Bit
      Convop_Bit_Log,

      Convop_Bit_Slv,   --  Bit to signed LV
      Convop_Bit_Ulv,
      Convop_Bit_Sbv,   --  Bit to signed BV
      Convop_Bit_Ubv,

      --  Fp
      Convop_Fp32_Fp64,
      Convop_Fp64_Fp32,
      Convop_Fp64_Ulv,  --  FP to logic vector
      Convop_Fp64_Slv,
      Convop_Fp64_Ubv,  --  FP to bit vector
      Convop_Fp64_Sbv
     );

   type Drive_Strength_Type is
     (
      Drive_Unknown,
      Drive_Highz,
      Charge_Small,
      Charge_Medium,
      Drive_Weak,
      Charge_Large,
      Drive_Pull,
      Drive_Strong,
      Drive_Supply
     );

   type Lifetime_Type is
     (
      Life_Static,
      Life_Automatic
     );

   type Visibility_Type is
     (
      Visibility_None,
      Visibility_Public,
      Visibility_Protected,
      Visibility_Local
     );

   type Edge_Type is
     (
      Edge_None,
      Edge_Posedge,
      Edge_Negedge,
      Edge_Any
     );

   type Base_Type is
     (
      Base_Binary,
      Base_Octal,
      Base_Decimal,
      Base_Hexa
     );

   type Polarity_Type is
     (
      Polarity_Unknown,
      Polarity_Positive,
      Polarity_Negative
     );

   type Join_Type is
     (
      Join_All,
      Join_Any,
      Join_None
     );

   type DPI_Spec_Type is
     (
      DPI_Unknown,
      DPI_DPI_C,
      DPI_DPI
     );

   type Violation_Type is
     (
      Violation_None,
      Violation_Unique,
      Violation_Unique0,
      Violation_Priority
     );

   --  An UDP (user defined primitive) can be:
   --  * combinational
   --  * level-sentitive sequential
   --  * edge-sensitive sequential
   type Udp_Kind is
     (
      Udp_Combinational,
      Udp_Level_Sensitive,
      Udp_Edge_Sensitive
     );

   subtype Udp_Sequential is Udp_Kind range
     Udp_Level_Sensitive ..
     Udp_Edge_Sensitive;

   type Udp_Symbol is
     (
      Udp_0,    --  0
      Udp_1,    --  1
      Udp_X,    --  x X
      Udp_Qm,   --  ?    (0, 1 or x)
      Udp_B,    --  b B  (0 or 1)
      Udp_R,    --  r R  (01) rising edge
      Udp_F,    --  f F  (10) falling edge
      Udp_P,    --  p P  (01), (0x), (x1)  potential positive edge
      Udp_N,    --  n N  (10), (1x), (x0)  potential falling edge
      Udp_Any,  --  *    (??) any value change
      Udp_No    --  -    no change
     );

   type Node is new Nat32;
   for Node'Size use 32;

   --  For easy mixing of languages.
   subtype Vlg_Node is Node;

   type Node_Array is array (Int32 range <>) of Node;
   type Node_Arr_Acc is access Node_Array;

   function Drive_Strength_To_Int32 (D0, D1 : Drive_Strength_Type)
                                   return Int32;
   function Get_Drive_Strength_0 (V : Int32) return Drive_Strength_Type;
   function Get_Drive_Strength_1 (V : Int32) return Drive_Strength_Type;

   type Sys_Tf_Id is new Int32;
   for Sys_Tf_Id'Size use 32;
   subtype Valid_Sys_Tf_Id is Sys_Tf_Id range 1 .. Sys_Tf_Id'Last;
   Bad_Sys_Tf_Id       : constant Sys_Tf_Id := -1;
   No_Sys_Tf_Id        : constant Sys_Tf_Id := 0;

   --  Predefined system functions.
   Sys_Tf_Signed_Id    : constant Sys_Tf_Id := 1;
   Sys_Tf_Unsigned_Id  : constant Sys_Tf_Id := 2;
   Sys_Tf_Cast_Id      : constant Sys_Tf_Id := 3;
   Sys_Tf_Typename_Id  : constant Sys_Tf_Id := 4;
   Sys_Tf_Left_Id      : constant Sys_Tf_Id := 5;
   Sys_Tf_Right_Id     : constant Sys_Tf_Id := 6;
   Sys_Tf_Low_Id       : constant Sys_Tf_Id := 7;
   Sys_Tf_High_Id      : constant Sys_Tf_Id := 8;
   Sys_Tf_Size_Id      : constant Sys_Tf_Id := 9;

   Sys_Tf_User_Id      : constant Sys_Tf_Id := 10;
   subtype User_Sys_Tf_Id is Sys_Tf_Id range Sys_Tf_User_Id .. Sys_Tf_Id'Last;

   --  Size of an unsized number.
   Unsized_Number : constant Int32 := -1;

   --  The next line marks the start of the node description.
   -- Start of Nkind.

   -- N_Compilation_Unit (Short)
   --  Corresponds to a compilation unit.
   --
   --  Always Name_D_Unit ($unit).
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --  List of description (module, udp).
   --   Get/Set_Descriptions (Field3)
   --
   --   Get/Set_Scope_Id (Field5)

   -- N_Timescale_Directive (Short)
   -- N_Timeunits_Declaration (Short)
   --
   --  Timeunits_declaration is synthesized from timeunit and/or timeprecision
   --  declarations.
   --
   --   Get/Set_Time_Unit (Field4)
   --
   --   Get/Set_Time_Precision (Field3)
   --
   --   Get/Set_Chain (Field2)

   -- N_Timeunit (Short)
   --
   --   Get/Set_Timeunit (Field3)
   --
   --   Get/Set_Timeprecision (Field4)
   --
   --   Get/Set_Chain (Field2)

   -- N_Timeprecision (Short)
   --
   --   Get/Set_Timeprecision (Field4)
   --
   --   Get/Set_Chain (Field2)

   -- N_Module (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Package_Import_Chain (Field10)
   --
   --   Get/Set_Parameter_Port_Chain (Field3)
   --
   --  List of ports or port_declarations.
   --   Get/Set_Ports_Chain (Field7)
   --
   --   Get/Set_Items_Chain (Field8)
   --
   --   Get/Set_Scope_Id (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Attributes_Chain (Field9)
   --
   --   Get/Set_Instantiated_Flag (Flag1)
   --
   --   Get/Set_Ansi_Port_Flag (Flag4)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Foreign_Module (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Parameter_Port_Chain (Field3)
   --
   --  List of ports or port_declarations.
   --   Get/Set_Ports_Chain (Field7)
   --
   --   Get/Set_Items_Chain (Field8)
   --
   --   Get/Set_Scope_Id (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Foreign_Node (Field4)
   --
   --   Get/Set_Instantiated_Flag (Flag1)
   --
   --   Get/Set_Ansi_Port_Flag (Flag4)

   -- N_Primitive (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Udp_Kind (State1)
   --
   --   Get/Set_Ports_Chain (Field7)
   --
   --   Get/Set_Udp_Port_Declaration_Chain (Field3)
   --
   --   Get/Set_Udp_Initial (Field4)
   --
   --   Get/Set_Udp_Entries_Chain (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Ansi_Port_Flag (Flag4)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Udp_Combinational_Entry (Short)
   --
   --   Get/Set_Input_Chain (Field1)
   --
   --   Get/Set_Output_Symbol (Field3)
   --
   --  Next entry
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Udp_Sequential_Entry (Short)
   --
   --   Get/Set_Input_Chain (Field1)
   --
   --   Get/Set_Current_State (Field3)
   --
   --   Get/Set_Next_State (Field4)
   --
   --  Next entry
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Udp_Level_Symbol (Short)
   --
   --   Get/Set_Symbol (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Udp_Change_Symbol (Short)
   --
   --   Get/Set_From_Symbol (Field1)
   --
   --   Get/Set_To_Symbol (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Interface_Declaration (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Package_Import_Chain (Field10)
   --
   --   Get/Set_Parameter_Port_Chain (Field3)
   --
   --   Get/Set_Scope_Id (Field5)
   --
   --  List of ports or port_declarations.
   --   Get/Set_Ports_Chain (Field7)
   --
   --   Get/Set_Items_Chain (Field8)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Instantiated_Flag (Flag1)
   --
   --   Get/Set_Ansi_Port_Flag (Flag4)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Class (Medium)
   -- N_Generic_Class (Medium)
   -- N_Instantiated_Class (Medium)
   --  An N_Class node is a class declaration that doesn't have parameters and
   --  does not extend a generic class.  It is therefore a type.
   --  Note: the parser may not distinguish a class from a generic class.
   --  An N_Generic_Class is a class declaration with parameters.  It isn't a
   --  type and must be instantiated to be a type.
   --  An N_Instantiated_Class is an instance of N_Generic_Class, and is
   --  therefore a type.
   --  An N_Class_Instance is a name that will result in an Instantiated_Class.
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Parameter_Port_Chain (Field3)
   --
   --   Get/Set_Base_Class_Type (Field4)
   --
   --   Get/Set_Scope_Id (Field5)
   --
   --   Get/Set_Class_Item_Chain (Field7)
   --
   --   Get/Set_Class_Constructor (Field8)
   --
   --  Not defined for N_Generic_Class, but still present because
   --  instantiated classes are created by cloning generic classes.
   --   Get/Set_Inheritance_Depth (Field9)
   --
   --  For instantiated class, this is the generic class.
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Has_Lifetime (Flag7)
   --
   --   Get/Set_Virtual_Flag (Flag12)
   --
   --  Owner flag for Base_Class_Type
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Has_End_Name (Flag5)
   --
   --  Maximal visibility allowed.  Must be set to Visibility_Local when the
   --  class is analyzed, Visibility_Protected when a descendent is analyzed,
   --  and Visibility_Public outside.
   --   Get/Set_Class_Visibility (State1)
   --
   --   Get/Set_Type_Analyzed_Flag (Flag1)
   --
   --   Get/Set_Size_Flag (Flag2)
   --
   --   Get/Set_Forward_Typedef_Flag (Flag4)
   --
   --  True if the class has extern methods.
   --   Get/Set_Has_Extern_Flag (Flag11)
   --
   --   Get/Set_Fully_Analyzed_Flag (Flag8)
   --
   --   Get/Set_Mark_Flag (Flag9)


   -- N_Program_Declaration (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Package_Import_Chain (Field10)
   --
   --   Get/Set_Parameter_Port_Chain (Field3)
   --
   --  List of ports or port_declarations.
   --   Get/Set_Ports_Chain (Field7)
   --
   --   Get/Set_Items_Chain (Field8)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Instantiated_Flag (Flag1)
   --
   --   Get/Set_Ansi_Port_Flag (Flag4)
   --
   --   Get/Set_Has_End_Name (Flag5)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Has_Lifetime (Flag7)

   -- N_Package (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Package_Item_Chain (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Has_Lifetime (Flag7)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Clocking (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Event (Field4)
   --
   --   Get/Set_Clocking_Item_Chain (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Property_Declaration (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Ports_Chain (Field7)
   --
   --   Get/Set_Items_Chain (Field8)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --  The property spec:
   --   Get/Set_Clocking_Event (Field9)
   --
   --   Get/Set_Disable_Expression (Field10)
   --
   --   Get/Set_Property_Expression (Field11)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Default_Clocking (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Event (Field4)
   --
   --   Get/Set_Clocking_Item_Chain (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Disable_Iff (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Default_Skew (Short)
   --
   --   Get/Set_Input_Skew (Field3)
   --
   --   Get/Set_Output_Skew (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Clocking_Skew (Short)
   --
   --   Get/Set_Delay_Control (Field1)
   --
   --   Get/Set_Edge_Identifier (State1)

   -- N_Clock_Var (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Input_Skew (Field3)
   --
   --   Get/Set_Output_Skew (Field5)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Modport (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Modport_Ports_Chain (Field3)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Modport_Input (Short)
   -- N_Modport_Output (Short)
   -- N_Modport_Inout (Short)
   -- N_Modport_Ref (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Modport_Clocking (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Modport_Import_Tf (Short)
   -- N_Modport_Export_Tf (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Port (Short)
   --
   --  port ::= [ port_expression ]
   --         | .port_identifier ( [ port_expression ] )
   --
   --  The port identifier, or Null_Node if not present (and if port expression
   --  is not an identifier).
   --   Get/Set_Identifier (Field1)
   --
   --  The port expression, or Null_Node if not present.  This is also the
   --  loconn.
   --   Get/Set_Expression (Field4)
   --
   --  The type of the port, deduced from the port expression.
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --  Next port.
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Connected_Flag (Flag4)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Input (Medium)
   -- N_Output (Short)
   -- N_Inout (Short)
   -- N_Tf_Input (Medium)
   -- N_Tf_Output (Medium)
   -- N_Tf_Inout (Medium)
   -- N_Tf_Ref (Medium)
   -- N_Tf_Const_Ref (Medium)
   --  Tf variant is for tasks/functions, where ports are not redeclared and
   --  are considered as variables.
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --  Ports can be redeclared as net or registers (either implicetly or
   --  explicitely).  This field contains that re-declaration.
   --   Get/Set_Redeclaration (Field4)
   --
   -- Only for N_Input:
   -- Only for N_Tf_Input:
   -- Only for N_Tf_Output:
   -- Only for N_Tf_Inout:
   -- Only for N_Tf_Ref:
   -- Only for N_Tf_Const_Ref:
   --   Get/Set_Default_Value (Field7)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Has_Identifier_List (Flag1)
   --
   --   Get/Set_Connected_Flag (Flag4)
   --
   --   Get/Set_Complete_Flag (Flag2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Has_Direction (Flag5)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Has_Attribute (Flag19)

   -- N_Interface_Port (Short)
   -- N_Modport_Port (Short)
   --  An ansi port declaration with an interface_port_header and no modport.
   --
   --   Get/Set_Identifier (Field1)
   --
   --  As a port can be an array, the interface name is considered as a type.
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Has_Identifier_List (Flag1)
   --
   --   Get/Set_Connected_Flag (Flag4)
   --
   --   Get/Set_Complete_Flag (Flag2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Constraint (Short)
   --
   --  1800-2017 18.5 Constraint blocks
   --  constraint_declaration ::=
   --    [static] CONSTRAINT constraint_identifier constraint_block
   --
   --  constraint_block ::= { { constraint_block_item } }
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Constraint_Block_Chain (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Constraint_Expression (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Constraint_If (Short)
   --
   --   Get/Set_Condition (Field5)
   --
   --   Get/Set_Cond_True (Field1)
   --
   --   Get/Set_Cond_False (Field4)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Constraint_Foreach (Short)
   --
   --   Get/Set_Foreach_Array (Field3)
   --
   --   Get/Set_Foreach_Variables (Field4)
   --
   --   Get/Set_Constraint_Set (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Typedef (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Forward_Typedef_Flag (Flag4)
   --
   --   Get/Set_Resolved_Flag (Flag8)
   --
   --   Get/Set_Mark_Flag (Flag9)

   -- N_Typedef_Class (Short)
   -- N_Typedef_Struct (Short)
   -- N_Typedef_Forward (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Forward_Type (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --  If false, this is the first declaration for the type.
   --   Get/Set_Forward_Typedef_Flag (Flag4)

   -- N_Predefined_Typedef (Short)
   --
   --  Typedef for a standard predefined type (like integer, int, byte, ...)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Task (Medium)
   -- N_Extern_Task (Medium)
   -- N_OOB_Task (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --  Statements are moved from the OOB task to the extern task.
   --   Get/Set_Statements_Chain (Field4)
   --
   --   Get/Set_Tf_Ports_Chain (Field7)
   --
   --   Get/Set_Tf_Item_Declaration_Chain (Field9)
   --
   --   Get/Set_This_Variable (Field11)
   --
   -- Only for N_OOB_Task:
   --   Get/Set_OOB_Prefix (Field10)
   --
   -- Only for N_Extern_Task:
   --   Get/Set_Out_Of_Block_Declaration (Field10)
   --
   --   Get/Set_Scope_Id (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Virtual_Flag (Flag12)
   --
   --   Get/Set_Pure_Flag (Flag1)
   --
   --   Get/Set_Has_Lifetime (Flag7)
   --
   --   Get/Set_Ansi_Port_Flag (Flag4)
   --
   --   Get/Set_Has_End_Name (Flag5)
   --
   --   Get/Set_Fully_Analyzed_Flag (Flag8)
   --
   --   Get/Set_Mark_Flag (Flag9)
   --
   --   Get/Set_Has_Visibility (Flag11)
   --
   --  Used only by methods, as this is different than lifetime.
   --   Get/Set_Static_Flag (Flag14)
   --
   --   Get/Set_Visibility (State1)

   -- N_Function (Medium)
   -- N_Extern_Function (Medium)
   -- N_OOB_Function (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Tf_Ports_Chain (Field7)
   --
   --   Get/Set_Return_Variable (Field8)
   --
   --   Get/Set_This_Variable (Field11)
   --
   --   Get/Set_Tf_Item_Declaration_Chain (Field9)
   --
   --   Get/Set_Statements_Chain (Field4)
   --
   -- Only for N_OOB_Function:
   --   Get/Set_OOB_Prefix (Field10)
   --
   -- Only for N_Extern_Function:
   --   Get/Set_Out_Of_Block_Declaration (Field10)
   --
   --   Get/Set_Scope_Id (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Ansi_Port_Flag (Flag4)
   --
   --   Get/Set_Has_End_Name (Flag5)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Virtual_Flag (Flag12)
   --
   --   Get/Set_Pure_Flag (Flag1)
   --
   --   Get/Set_Has_Lifetime (Flag7)
   --
   --   Get/Set_Fully_Analyzed_Flag (Flag8)
   --
   --   Get/Set_Mark_Flag (Flag9)
   --
   --   Get/Set_Has_Visibility (Flag11)
   --
   --   Get/Set_Visibility (State1)
   --
   --  Used only by methods, as this is different than lifetime.
   --   Get/Set_Static_Flag (Flag14)

   -- N_Import_DPI_Function (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_C_Identifier (Field4)
   --
   --   Get/Set_Tf_Ports_Chain (Field7)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Pure_Property (Flag1)
   --
   --   Get/Set_Context_Property (Flag2)
   --
   --  Should always be set.
   --   Get/Set_Ansi_Port_Flag (Flag4)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Fully_Analyzed_Flag (Flag8)
   --
   --   Get/Set_Mark_Flag (Flag9)
   --
   --   Get/Set_DPI_Spec (State1)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Export_DPI_Function (Short)
   -- N_Export_DPI_Task (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_C_Identifier (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_DPI_Spec (State1)

   -- N_Specify (Short)
   --
   --   Get/Set_Specify_Item_Chain (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Chain (Field2)

   -- N_Ifnone (Short)
   --
   --   Get/Set_True_Stmt (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Timing_Check (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Arguments (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Wire_Direct (Short)
   --  Same as N_Wire but without delay/strength. The node is smaller.
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Has_Identifier_List (Flag1)
   --
   --   Get/Set_Implicit_Flag (Flag2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Redeclaration_Flag (Flag5)
   --
   --  Always false.
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Wire (Medium)
   -- N_Tri (Medium)
   -- N_Wand (Medium)
   -- N_Triand (Medium)
   -- N_Wor (Medium)
   -- N_Trior (Medium)
   -- N_Tri0 (Medium)
   -- N_Tri1 (Medium)
   -- N_Supply0 (Medium)
   -- N_Supply1 (Medium)
   -- N_Uwire (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Net_Delay (Field8)
   --
   --   Get/Set_Net_Drive_Strength (Field9)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Has_Identifier_List (Flag1)
   --
   --   Get/Set_Implicit_Flag (Flag2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Redeclaration_Flag (Flag5)
   --
   --  Always false.
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Trireg (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Charge_Strength (Field9)
   --
   --   Get/Set_Net_Delay (Field8)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Has_Identifier_List (Flag1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --  Always false.
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Var (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Has_Identifier_List (Flag1)
   --
   --   Get/Set_Is_Const (Flag2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --  True iff 'var' keyword is present.
   --   Get/Set_Has_Var (Flag4)
   --
   --   Get/Set_Redeclaration_Flag (Flag5)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Has_Lifetime (Flag7)
   --
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Has_Visibility (Flag11)
   --
   --   Get/Set_Visibility (State1)
   --
   --   Get/Set_Random_Flag (Flag12)
   --
   --   Get/Set_Randc_Flag (Flag13)
   --
   --  Used only for attributes.
   --   Get/Set_Static_Flag (Flag14)

   -- N_Return_Var (Short)
   -- N_This_Var (Short)
   --
   --  These variables are implicit (not user declared).  No declared type,
   --  no visibility...
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Parameter (Medium)
   -- N_Specparam (Medium)
   -- N_Localparam (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --  Maybe null for N_Parameter.
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --  The node that defines/overrides the value.  Either a defparam or
   --  a parameter_value_expr.
   -- Only for N_Parameter:
   --   Get/Set_Override_Stmt (Field7)
   --
   --  The final value: either Expression, or from Override_Stmt.
   -- Only for N_Parameter:
   --   Get/Set_Parameter_Expression (Field8)
   --
   --  Type of the parameter.
   --   Get/Set_Param_Type (Field9)
   --
   --  For AMS: the value range (a chain)
   --   Get/Set_Value_Range (Field10)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)
   --
   --  Always false.
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Fully_Analyzed_Flag (Flag8)
   --
   --   Get/Set_Mark_Flag (Flag9)

   -- N_Pulse_Control_Specparam (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Reject_Limit (Field4)
   --
   --   Get/Set_Error_Limit (Field7)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --  Type of the parameter.
   --   Get/Set_Param_Type (Field9)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)
   --
   --  Always false.
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Fully_Analyzed_Flag (Flag8)
   --
   --   Get/Set_Mark_Flag (Flag9)

   -- N_From_Range (Short)
   -- N_Exclude_Range (Short)
   --
   --  The first (and smallest) end point.
   --   Get/Set_Lsb (Field6)
   --
   --  The second (and biggest) end point.
   --  Not set for a single value or for strings array.
   --   Get/Set_Msb (Field5)
   --
   --   Get/Set_Lsb_Include_Flag (Flag1)
   --
   --   Get/Set_Msb_Include_Flag (Flag2)
   --
   --   Get/Set_Chain (Field2)

   -- N_Type_Parameter (Short)
   -- N_Type_Localparam (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Default_Type (Field3)
   --
   --  The final type.
   -- Only for N_Type_Parameter:
   --   Get/Set_Parameter_Type (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --  Owner flag for Default_Type
   --   Get/Set_Type_Owner (Flag3)
   --
   --  Set if 'type' keyword is present.
   --   Get/Set_Has_Type (Flag4)

   -- N_Package_Import (Short)
   --
   --   Get/Set_Item_Name (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Genvar (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expression (Field4)
   --
   --  The current value of the genvar, for evaluation.
   --   Get/Set_Generate_Index (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Assign (Short)
   -- N_Decl_Assign (Short)
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Assign_Delay (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Drive_Strength (Field5)
   --
   --   Get/Set_Parent (Field6)

   -- N_Always (Short)
   -- N_Always_Comb (Short)
   -- N_Always_Latch (Short)
   -- N_Always_Ff (Short)
   -- N_Initial (Short)
   -- N_Final (Short)
   -- N_Debug (Short)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Process_Id (Field5)
   --
   --   Get/Set_Parent (Field6)

   -- N_Discipline (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Discipline_Items (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Branch (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Arg1 (Field4)
   --
   --   Get/Set_Arg2 (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Port_Branch (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Port (Field3)

   -- N_Nature (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Nature_Items (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Nature_Attribute (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Nature_Access (Short)
   --  The access function.  Note that contrary to N_Nature_Attribute, the
   --  identifier is the name of the function.
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Discipline_Domain (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Continuous_Flag (Flag1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Discipline_Potential (Short)
   -- N_Discipline_Flow (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Nature (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Discipline_Attribute (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Potential_Flag (Flag1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Analog (Short)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Assert_Property (Medium)
   -- N_Assume_Property (Medium)
   --
   --  1800-2017 16.14 Concurrent assertions
   --  assert_property_statement ::=
   --    ASSERT PROPERTY ( property_spec ) action_block
   --  assume_property_statement ::=
   --    ASSUME PROPERTY ( property_spec ) action_block
   --
   --  1800-2017 16.12 Declaring properties
   --  property_spec ::=
   --    [ clocking_event ][ DISABLE IFF ( expression_or_dist ) ] property_expr
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Process_Id (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --  The property spec:
   --   Get/Set_Clocking_Event (Field9)
   --
   --   Get/Set_Disable_Expression (Field10)
   --
   --   Get/Set_Property_Expression (Field11)
   --
   --   Get/Set_Pass_Stmt (Field4)
   --
   --   Get/Set_Else_Stmt (Field3)

   -- N_Module_Instance (Medium)
   -- N_Program_Instance (Medium)
   --
   --  Name of the module/program to be instantiated
   --   Get/Set_Module (Field7)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Range (Field3)
   --
   --  Chain of N_Parameter_Value_Expr
   --   Get/Set_Parameter_Values (Field9)
   --
   --  Chain of Port_Connection.
   --   Get/Set_Connections (Field8)
   --
   --  Fully expanded/instantiated module.
   --   Get/Set_Instance (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Interface_Instance (Medium)
   --
   --   Get/Set_Interface_Name (Field7)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Range (Field3)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --  Chain of N_Parameter_Value_Expr
   --   Get/Set_Parameter_Values (Field9)
   --
   --  Chain of Port_Connection.
   --   Get/Set_Connections (Field8)
   --
   --   Get/Set_Instance_Ref (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Class_Instance (Medium)
   --
   --   Get/Set_Class_Name (Field5)
   --
   --  Chain of N_Parameter_Value_Expr
   --   Get/Set_Parameter_Values (Field9)
   --
   --  The interned type.
   --   Get/Set_Expr_Type (Field3)
   --
   --  Also the interned type.
   --   Get/Set_Declaration (Field4)

   -- N_Virtual_Interface (Medium)
   --
   --   Get/Set_Interface (Field3)
   --
   --  Chain of N_Parameter_Value_Expr
   --   Get/Set_Parameter_Values (Field9)
   --
   --   Get/Set_Instance (Field4)

   -- N_Gate_And (Medium)
   -- N_Gate_Nand (Medium)
   -- N_Gate_Or (Medium)
   -- N_Gate_Nor (Medium)
   -- N_Gate_Xor (Medium)
   -- N_Gate_Xnor (Medium)
   -- N_Gate_Buf (Medium)
   -- N_Gate_Not (Medium)
   -- N_Gate_Bufif0 (Medium)
   -- N_Gate_Bufif1 (Medium)
   -- N_Gate_Notif0 (Medium)
   -- N_Gate_Notif1 (Medium)
   -- N_Gate_Nmos (Medium)
   -- N_Gate_Pmos (Medium)
   -- N_Gate_Rnmos (Medium)
   -- N_Gate_Rpmos (Medium)
   -- N_Gate_Tran (Medium)
   -- N_Gate_Rtran (Medium)
   -- N_Gate_Tranif0 (Medium)
   -- N_Gate_Tranif1 (Medium)
   -- N_Gate_Rtranif0 (Medium)
   -- N_Gate_Rtranif1 (Medium)
   -- N_Gate_Cmos (Medium)
   -- N_Gate_Rcmos (Medium)
   -- N_Gate_Pullup (Medium)
   -- N_Gate_Pulldown (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Drive_Strength (Field5)
   --
   --   Get/Set_Gate_Delay (Field4)
   --
   --   Get/Set_Range (Field3)
   --
   --   Get/Set_Gate_Terminals (Field8)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Primitive_Instance (Medium)
   --
   --   Get/Set_Module (Field7)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Drive_Strength (Field5)
   --
   --   Get/Set_Gate_Delay (Field4)
   --
   --   Get/Set_Gate_Terminals (Field8)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Control_Terminal (Short)
   -- N_Input_Terminal (Short)
   -- N_Inout_Terminal (Short)
   -- N_Output_Terminal (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Chain (Field2)


   -- N_Port_Connection (Short)
   -- N_Implicit_Connection (Short)
   --
   --  Port identifier.
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Port (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Collapse_Flag (Flag1)

   -- N_Wildcard_Connection (Short)
   --
   --   Get/Set_Chain (Field2)

   -- N_Default_Connection (Short)
   --  Connection using the default value.
   --
   --   Get/Set_Port (Field3)
   --
   --   Get/Set_Chain (Field2)

   -- N_Parameter_Value_Expr (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expression (Field4)
   --
   --  The parameter to be set.
   --   Get/Set_Parameter (Field5)
   --
   --   Get/Set_Chain (Field2)

   -- N_Parameter_Value_Type (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --  The parameter to be set.
   --   Get/Set_Parameter (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Defparam (Short)
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Parameter (Field5)
   --
   --   Get/Set_Parent (Field6)

   -- N_Generate_Region (Short)
   --
   --  generate_region ::= GENERATE { generate_item } ENDGENERATE
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Generate_Item_Chain (Field4)
   --
   --   Get/Set_Parent (Field6)

   -- N_Generate_Block (Short)
   -- N_Array_Generate_Block (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Generate_Item_Chain (Field4)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Indexed_Generate_Block (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Generate_Index (Field5)
   --
   --   Get/Set_Generate_Item_Chain (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_If_Generate (Short)
   --
   --  After elaboration, the selected block is appended just after this
   --  statement (which therefore should be ignored).
   --   Get/Set_Condition (Field5)
   --
   --   Get/Set_True_Block (Field3)
   --
   --   Get/Set_False_Block (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Loop_Generate (Short)
   --
   --   Get/Set_For_Initialization (Field3)
   --
   --   Get/Set_Condition (Field5)
   --
   --   Get/Set_Step_Assign (Field4)
   --
   --  An N_Array_Generate_Block containing N_Indexed_Generate_Block.
   --   Get/Set_Generate_Block (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Case_Generate (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Case_Items (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Simple_Immediate_Assert (Short)
   --
   --   Get/Set_Condition (Field5)
   --
   --   Get/Set_Pass_Stmt (Field4)
   --
   --   Get/Set_Else_Stmt (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Seq_Block (Medium)
   -- N_Par_Block (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Block_Item_Declaration_Chain (Field3)
   --
   --   Get/Set_Statements_Chain (Field4)
   --
   --   Get/Set_Attributes_Chain (Field9)
   --
   --  FIXME: Is it needed ?
   --   Get/Set_Scope_Id (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   -- Only for N_Par_Block:
   --   Get/Set_Join_Option (State1)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Has_End_Name (Flag5)

   -- N_Label_Stmt (Short)
   --  1800-2017 9.3.5 Statement labels
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --  Use a chain to behave like a seq block, but there is only one stmt.
   --   Get/Set_Statements_Chain (Field4)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)

   -- N_If (Short)
   --
   --   Get/Set_Condition (Field5)
   --
   --   Get/Set_True_Stmt (Field3)
   --
   --   Get/Set_False_Stmt (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Violation (State1)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)

   -- N_For (Short)
   --
   --   Get/Set_For_Initialization (Field3)
   --
   --   Get/Set_Condition (Field5)
   --
   --   Get/Set_Step_Assign (Field4)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Scope_Flag (Flag1)

   -- N_While (Short)
   -- N_Do_While (Short)
   --
   --   Get/Set_Condition (Field5)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Foreach (Short)
   --
   --   Get/Set_Foreach_Array (Field3)
   --
   --   Get/Set_Foreach_Variables (Field4)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Foreach_Variable (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Repeat (Short)
   --
   --  Also looks like a variable to store the count number.
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --  A variable is needed to count the number of iterations.
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Forever (Short)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Parent (Field6)

   -- N_Disable (Short)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Disable_Fork (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Wait (Short)
   --
   --   Get/Set_Condition (Field5)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Wait_Fork (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Trigger (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Event (Field4)
   --
   --   Get/Set_Parent (Field6)

   -- N_Noblk_Assign (Short)
   -- N_Blocking_Assign (Short)
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Control (Field5)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Force_Assign (Short)
   -- N_Proc_Assign (Short)
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Release (Short)
   -- N_Proc_Deassign (Short)
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Unpack_Assign (Short)
   -- N_Pack_Assign (Short)
   -- N_Pack_Unpack_Assign (Short)
   --
   --  1800-2017 10.10 Unpacked array concatenation
   --  1800-2017 11.4.14.3 Streaming concatenation as an assignment target
   --
   --  Like a blocking assignment, but LHS and/or RHS are streaming
   --  concatenation.  Contrary to normal assignment, they can only appear at
   --  particular places, and they don't have a type.
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Control (Field5)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Assign_Operator (Short)
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Binary_Op (Field5)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Pre_Increment (Short)
   -- N_Post_Increment (Short)
   -- N_Pre_Decrement (Short)
   -- N_Post_Decrement (Short)
   --  As these operators can be used as statements, they have chain/parent.
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_New_Call (Short)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Arguments (Field4)
   --
   --   Get/Set_Has_Parenthesis (Flag1)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_New_Expression (Short)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Dynamic_Array_New (Short)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Size_Expression (Field5)
   --
   --   Get/Set_Init_Expression (Field4)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Type_Cast (Short)
   --
   --  cast ::= casting_type ' ( expression )
   --
   --  Note: according to the grammer, the data type is a keyword or an
   --  identifier.
   --   Get/Set_Cast_Data_Type (Field2)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Conversion_Op (Field1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Parenthesis_Expr (Short)
   -- N_Size_Cast (Short)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   -- Only for N_Size_Cast:
   --   Get/Set_Size_Expression (Field5)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Conversion_Op (Field1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Aggregate_Literal (Short)
   --
   --  Only set for multiple concatenations.
   --   Get/Set_Replication (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Elements (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Aggregate_Literal_Cst (Short)
   --
   --   Get/Set_Replication_Cst (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Elements (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Aggregate_Element (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Pattern_Key (Field5)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Null (Short)
   -- N_Default (Short)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_This (Short)
   -- N_Super (Short)
   --
   --  The reference to the N_This_Variable.
   --   Get/Set_Declaration (Field4)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Event_Control (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --  Note: only for procedural_timing_control_statement.
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Delay_Control (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --  Note: only for procedural_timing_control_statement.
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Timescale (Field5)
   --
   --   Get/Set_Parent (Field6)

   -- N_Repeat_Control (Short)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Control (Field5)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Parent (Field6)

   -- N_Cycle_Delay (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --  Note: only for procedural_timing_control_statement.
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Case (Short)
   -- N_Casex (Short)
   -- N_Casez (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Case_Items (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Violation (State1)
   --
   --   Get/Set_Lifetime (Flag6)
   --
   --   Get/Set_Is_Automatic (Flag10)
   --
   --   Get/Set_Has_Attribute (Flag19)
   --
   --  True if the statement has the full_case attribute (set to 1).
   --   Get/Set_Attribute_Full (Flag1)
   --
   --  True if the statement has the parallel_case attribute (set to 1).
   --   Get/Set_Attribute_Parallel (Flag2)
   --
   --  True if the statement has other attributes than full/parallel or
   --  full/parallel with a value.
   --   Get/Set_Other_Attributes (Flag3)

   -- N_Case_Item (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Statement (Field1)
   --
   --  This case item is followed by another case item and share the same
   --  statements.  The statements are on the last case item (whose
   --  same_case_flag is not set).
   --   Get/Set_Same_Case_Flag (Flag1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Lifetime (Flag6)

   -- N_Default_Case_Item (Short)
   --
   --   Get/Set_Statement (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Lifetime (Flag6)

   -- N_Subroutine_Call_Stmt (Short)
   --
   --   Get/Set_Call (Field1)
   --
   --   Get/Set_Has_Void_Cast (Flag1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)

   -- N_Return_Stmt (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Chain (Field2)
   --
   --  Reference to the return variable.
   --   Get/Set_Return_Variable_Ref (Field3)
   --
   --   Get/Set_Parent (Field6)

   -- N_Break_Stmt (Short)
   -- N_Continue_Stmt (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Parent (Field6)


   -- N_Contribution (Short)
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Parent (Field6)


   -- N_Argument (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Port (Field3)

   -- N_Iterator_Argument (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Obj_Id (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --  Always false.
   --   Get/Set_Is_Automatic (Flag10)

   -- N_Array_Method_Call (Short)
   --
   --   Get/Set_Subroutine (Field1)
   --
   --  Set only for method calls.
   --   Get/Set_Object (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Iterator_Argument (Field5)
   --
   --   Get/Set_With_Expression (Field6)
   --
   --   Get/Set_Has_Parenthesis (Flag1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --  True if iterator_argument is specified, otherwise it's a default one.
   --   Get/Set_Has_Argument (Flag2)

   -- N_Call (Short)
   -- N_Randomize_Call (Short)
   --
   --  Name of the subroutine.
   --   Get/Set_Subroutine (Field1)
   --
   --  Set only for method calls.
   --   Get/Set_Object (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Arguments (Field4)
   --
   -- Only for N_Randomize_Call:
   --   Get/Set_With_Expression (Field6)
   --
   -- Only for N_Randomize_Call:
   --   Get/Set_Constraint_Block_Chain (Field5)
   --
   --   Get/Set_Has_Parenthesis (Flag1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Access_Call (Short)
   --  Name of the access identifier
   --   Get/Set_Access (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Arg1 (Field4)
   --
   --   Get/Set_Arg2 (Field5)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_System_Call (Short)
   --  Either a function or a task call.
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Call_Scope (Field2)
   --
   --   Get/Set_Sys_Tf_Id (Field5)
   --
   --   Get/Set_Arguments (Field4)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Has_Parenthesis (Flag1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Bits_Expr (Short)
   --  Node for $bits with an expression as argument.
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Bits_Type (Short)
   --  Node for $bits with a type as argument.
   --
   --   Get/Set_Type_Argument (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --  For Type_Argument
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Posedge (Short)
   -- N_Negedge (Short)
   --
   --   Get/Set_Expression (Field4)

   -- N_Or (Short)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field4)

   -- N_Delay (Short)
   --
   --   Get/Set_Rising_Delay (Field1)
   --
   --   Get/Set_Falling_Delay (Field2)
   --
   --   Get/Set_Highz_Delay (Field3)

   -- N_Logic_Type (Short)
   -- N_Bit_Type (Short)
   -- N_Real_Type (Short)
   -- N_Shortreal_Type (Short)
   -- N_Error_Type (Short)
   --
   --  There are 2 bit types and 2 logic types: one signed, one unsigned.
   --  There are 2 float types: real and shortreal.
   --  Note: Error_Type is considered as an integral type.
   --
   --   Get/Set_Signed_Flag (Flag1)
   --
   --  For integral types, the number of bits.
   --   Get/Set_Type_Width (Field4)
   --
   --   Get/Set_Size_Flag (Flag2)

   -- N_Enum_Type (Short)
   --
   --  The base type as it is declared.
   --   Get/Set_Enum_Base_Data_Type (Field5)
   --
   --   Get/Set_Enum_Names (Field1)
   --
   --  The type itself.
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Width (Field4)
   --
   --   Get/Set_Signed_Flag (Flag1)
   --
   --  Enum base type.  Either the declared one or the implicit int.
   --   Get/Set_Enum_Base_Type (Field2)
   --
   --  Owner flag for Base_Type
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Size_Flag (Flag2)

   -- N_Enum_Name (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Parent (Field6)

   -- N_Enum_Range (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --  MSB and LSB are expressions.
   --   Get/Set_Msb (Field5)
   --
   --   Get/Set_Lsb (Field6)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_String_Type (Short)
   -- N_Chandle_Type (Short)
   -- N_Event_Type (Short)
   -- N_Void_Type (Short)
   -- N_Null_Type (Short)
   --
   --  There is only one instance of these nodes.
   --
   --   Get/Set_Size_Flag (Flag2)

   -- N_Wildcard_Type (Short)
   --  A very special type; there are only a limited number of wildcard types,
   --  defined in verilog-standard.

   -- N_Packed_Array (Short)
   -- N_Array (Short)
   --
   --  MSB and LSB are expressions.
   --   Get/Set_Msb (Field5)
   --
   --   Get/Set_Lsb (Field6)
   --
   --   Get/Set_Element_Data_Type (Field2)
   --
   --   Get/Set_Signed_Flag (Flag1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Has_Sign (Flag4)
   --
   --  Owner flag for Element_Data_Type
   --   Get/Set_Type_Owner (Flag3)

   -- N_Log_Packed_Array_Cst (Short)
   -- N_Bit_Packed_Array_Cst (Short)
   -- N_Array_Cst (Short)
   --
   --  MSB and LSB are values.
   --   Get/Set_Msb_Cst (Field5)
   --
   --   Get/Set_Lsb_Cst (Field6)
   --
   --   Get/Set_Type_Element_Type (Field2)
   --
   --  For integral types, the number of bits.
   -- Only for N_Log_Packed_Array_Cst:
   -- Only for N_Bit_Packed_Array_Cst:
   --   Get/Set_Type_Width (Field4)
   --
   -- Only for N_Array_Cst:
   --   Get/Set_Type_Size (Field4)
   --
   -- Only for N_Log_Packed_Array_Cst:
   -- Only for N_Bit_Packed_Array_Cst:
   --   Get/Set_Stride_Width (Field1)
   --
   -- Only for N_Array_Cst:
   --   Get/Set_Stride_Size (Field1)
   --
   --   Get/Set_Signed_Flag (Flag1)
   --
   --   Get/Set_Size_Flag (Flag2)

   -- N_Struct_Type (Short)
   -- N_Union_Type (Short)
   --
   --   Get/Set_Members (Field1)
   --
   --   Get/Set_Nbr_Members (Field2)
   --
   --   Get/Set_Type_Size (Field4)
   --
   --  The type itself.
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Scope_Id (Field5)
   --
   --   Get/Set_Size_Flag (Flag2)

   -- N_Packed_Struct_Type (Short)
   -- N_Packed_Union_Type (Short)
   --
   --   Get/Set_Members (Field1)
   --
   --   Get/Set_Nbr_Members (Field2)
   --
   --   Get/Set_Signed_Flag (Flag1)
   --
   --   Get/Set_Has_Sign (Flag4)
   --
   --  The type itself.
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Packed_Base_Type (Field5)
   --
   --   Get/Set_Type_Width (Field4)
   --
   --   Get/Set_Size_Flag (Flag2)

   -- N_Member (Medium)
   -- N_Packed_Member (Medium)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Data_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   -- Only for N_Member:
   --   Get/Set_Obj_Id (Field5)
   --
   -- Only for N_Packed_Member:
   --   Get/Set_Packed_Member_Offset (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --  Member index, from 1 to nbr members.
   --   Get/Set_Member_Index (Field7)
   --
   --   Get/Set_Has_Identifier_List (Flag1)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Attribute (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expression (Field4)
   --
   --  Node to which the attribute applies.
   --   Get/Set_Attribute_Item (Field3)

   -- N_Queue (Short)
   --
   --  Null_Node for unbounded queue.
   --   Get/Set_Maximum_Size_Expr (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Element_Data_Type (Field2)
   --
   --  Owner flag for Element_Data_Type
   --   Get/Set_Type_Owner (Flag3)

   -- N_Queue_Cst (Short)
   --
   --  Set to -1 for unlimited queue.
   --   Get/Set_Maximum_Size_Cst (Field1)
   --
   --   Get/Set_Type_Element_Type (Field2)
   --
   --   Get/Set_Size_Flag (Flag2)

   -- N_Associative_Array (Short)
   --
   --  Null for wildcard.
   --   Get/Set_Index_Data_Type (Field1)
   --
   --   Get/Set_Element_Data_Type (Field2)
   --
   --  The interned type.
   --   Get/Set_Expr_Type (Field3)
   --
   --  Owner flag for Element_Data_Type
   --   Get/Set_Type_Owner (Flag3)
   --
   --  Owner flag for Index_Data_Type
   --   Get/Set_Type_Owner_2 (Flag5)

   -- N_Associative_Array_Cst (Short)
   --
   --  Null for wildcard.
   --   Get/Set_Type_Index_Type (Field1)
   --
   --   Get/Set_Type_Element_Type (Field2)
   --
   --   Get/Set_Size_Flag (Flag2)

   -- N_Dynamic_Array_Cst (Short)
   --
   --  Interned form of N_Dynamic_Array
   --
   --   Get/Set_Stride_Size (Field1)
   --
   --   Get/Set_Type_Element_Type (Field2)
   --
   --   Get/Set_Size_Flag (Flag2)

   -- N_Dynamic_Array (Short)
   --
   --   Get/Set_Element_Data_Type (Field2)
   --
   --  The interned type.
   --   Get/Set_Expr_Type (Field3)
   --
   --  Owner flag for Element_Data_Type
   --   Get/Set_Type_Owner (Flag3)

   -- N_Part_Select (Short)
   -- N_Slice_Name (Short)
   --
   --   Get/Set_Msb (Field5)
   --
   --   Get/Set_Lsb (Field6)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Name (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Plus_Part_Select (Short)
   -- N_Minus_Part_Select (Short)
   --
   --   Get/Set_Base_Expr (Field4)
   --
   --   Get/Set_Width_Expr (Field5)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Name (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Part_Select_Cst (Short)
   -- N_Slice_Name_Cst (Short)
   --
   --   Get/Set_Msb_Cst (Field5)
   --
   --   Get/Set_Lsb_Cst (Field6)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Name (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Plus_Part_Select_Cst (Short)
   -- N_Minus_Part_Select_Cst (Short)
   --
   --   Get/Set_Base_Expr (Field4)
   --
   --   Get/Set_Width_Cst (Field5)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Name (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Bit_Select (Short)
   -- N_Indexed_Name (Short)
   -- N_String_Index (Short)
   -- N_Associative_Index (Short)
   --
   --  N_String_Index is an index for a string, but it is not a name.
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Name (Field2)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Member_Select (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Name (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Value_Range (Short)
   --
   --   Get/Set_Msb (Field5)
   --
   --   Get/Set_Lsb (Field6)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Concatenation (Short)
   --
   --  Only set for multiple concatenations.
   --   Get/Set_Replication (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --  Chain of N_Element to be concatenated
   --   Get/Set_Expressions (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Membership (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --  Chain of N_Element to be concatenated
   --   Get/Set_Expressions (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Replication_Cst (Short)
   --
   --   Get/Set_Replication_Cst (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --  Chain of N_Element to be concatenated
   --   Get/Set_Expressions (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Element (Short)
   --  Form the chain of expressions (for concatenation, range list)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expression (Field4)

   -- N_Left_Streaming_Expr (Short)
   -- N_Right_Streaming_Expr (Short)
   --
   --   Get/Set_Expression (Field4)
   --
   --  Chain of concatenation_element to be concatenated
   --   Get/Set_Expressions (Field2)

   -- N_Left_Streaming_Type (Short)
   -- N_Right_Streaming_Type (Short)
   --
   --   Get/Set_Slice_Size_Type (Field4)
   --
   --  Chain of concatenation_element to be concatenated
   --   Get/Set_Expressions (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --  Owner flag for Slice_Size_Type
   --   Get/Set_Type_Owner_2 (Flag5)

   -- N_Stream_Expression (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Expression (Field4)

   -- N_Cond_Op (Short)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Cond_True (Field1)
   --
   --   Get/Set_Cond_False (Field4)
   --
   --   Get/Set_Condition (Field5)
   --
   --   Get/Set_Op_Attributes (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Unary_Op (Short)
   --
   --   Get/Set_Unary_Op (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Op_Attributes (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Binary_Op (Short)
   -- N_Short_Circuit_Op (Short)
   --
   --   Get/Set_Binary_Op (Field5)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field4)
   --
   --   Get/Set_Op_Attributes (Field2)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Conversion (Short)
   --
   --   Get/Set_Conversion_Op (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Expression (Field4)
   --
   --   Get/Set_Type_Owner (Flag3)

   -- N_Seq_Parenthesis (Short)
   --
   --   Get/Set_Sequence (Field4)

   -- N_Seq_Repeat (Short)
   --
   --   Get/Set_Sequence (Field4)
   --
   --   Get/Set_Msb (Field5)
   --
   --   Get/Set_Lsb (Field6)

   -- N_Seq_Plus_Repeat (Short)
   -- N_Seq_Star_Repeat (Short)
   --
   --   Get/Set_Sequence (Field4)

   -- N_Seq_Star_Concat (Short)
   -- N_Seq_Plus_Concat (Short)
   -- N_Seq_Throughout (Short)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field4)

   -- N_Seq_Const_Concat (Short)
   -- N_Seq_Range_Concat (Short)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field4)
   --
   --   Get/Set_Repeat_Expression (Field5)

   -- N_Prop_Not (Short)
   --
   --   Get/Set_Expression (Field4)

   -- N_Prop_Or (Short)
   -- N_Prop_And (Short)
   -- N_Prop_Overlap_Imp (Short)
   -- N_Prop_Non_Overlap_Imp (Short)
   -- N_Prop_Until (Short)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field4)

   -- N_Error_Expr (Short)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Error_Origin (Field1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Name (Short)
   -- N_This_Name (Short)
   --
   --  A name in the source.  If the name refers to a property or a method,
   --  the node N_This_Name is used to keep the reference to the implicit
   --  'this' parameter.
   --
   --   Get/Set_Identifier (Field1)
   --
   -- Only for N_This_Name:
   --   Get/Set_This_Declaration (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Declaration (Field4)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Dotted_Name (Short)
   -- N_Member_Name (Short)
   -- N_Hierarchical (Short)
   -- N_Property_Name (Short)
   -- N_Method_Name (Short)
   -- N_Class_Qualified_Name (Short)
   -- N_Interface_Item (Short)
   -- N_Modport_Item (Short)
   -- N_Scoped_Name (Short)
   --
   --  Dotted name is the node created by the parser, but during analysis
   --  it is changed to a more precise node:
   --  * N_Hierarchical for a hierarchical name
   --  * N_Member_Name for selecting a structure member.
   --  * N_Class_Qualified_Name for selecting an enum/parameter of a class.
   --  * N_Property_Name for a class property, prefix is an object.
   --  * N_Method_Name for a method call.
   --
   --  Scoped name is created by the parser, as it has a different syntax.
   --  It is here because it has the same fields as the other ones.
   --
   --   Get/Set_Name (Field2)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Declaration (Field4)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Wildcard_Name (Short)
   --
   --   Get/Set_Name (Field2)

   -- N_Number (Short)
   -- N_Computed_Number (Short)
   --
   --   Get/Set_Number_Hi_Val (Field1)
   --
   --   Get/Set_Number_Lo_Val (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Number_Hi_Zx (Field4)
   --
   --   Get/Set_Number_Lo_Zx (Field5)
   --
   --  The size as indicated by the user, but 0 for unsized numbers.
   -- Only for N_Number:
   --   Get/Set_Number_Size (Field6)
   --
   -- Only for N_Computed_Number:
   --   Get/Set_Expr_Origin (Field6)
   --
   --  The base as indicated by the user.  This is Base_Decimal for all
   --  decimal_number.
   --   Get/Set_Number_Base (State1)
   --
   --   Get/Set_Signed_Flag (Flag1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Bignum (Short)
   --
   --   Get/Set_Bignum_Index (Field1)
   --
   --   Get/Set_Bignum_Len (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --  The size as indicated by the user, but 0 for unsized numbers.
   --   Get/Set_Number_Size (Field6)
   --
   --  The base as indicated by the user.  This is Base_Decimal for all
   --  decimal_number.
   --   Get/Set_Number_Base (State1)
   --
   --   Get/Set_Signed_Flag (Flag1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Unbased_Literal (Short)
   --
   --   Get/Set_Number_Lo_Val (Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Number_Lo_Zx (Field5)
   --
   --   Get/Set_Signed_Flag (Flag1)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Time_Literal (Short)
   --
   --   Get/Set_Real_Number (Field1,Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Time_Unit (Field4)
   --
   --   Get/Set_Timescale (Field5)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_1step_Literal (Short)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Infinity (Short)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Real_Number (Short)
   --
   --   Get/Set_Real_Number (Field1,Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Scale_Number (Short)
   --
   --   Get/Set_Real_Number (Field1,Field2)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --  The power of 10.
   --   Get/Set_Scale_Factor (Field4)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Mintypmax (Short)
   --
   --   Get/Set_Min_Expr (Field1)
   --
   --   Get/Set_Typ_Expr (Field2)
   --
   --   Get/Set_Max_Expr (Field4)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_String_Literal (Short)
   --
   --   Get/Set_String_Id (Field1)
   --
   --   Get/Set_Expr_Type (Field3)
   --
   --  Length of the string, in bytes.
   --   Get/Set_String_Size (Field6)
   --
   --   Get/Set_Lit_Id (Field5)
   --
   --   Get/Set_Type_Owner (Flag3)
   --
   --   Get/Set_Is_Constant (Flag4)

   -- N_Implicit_Event (Short)
   --  Corresponds to @*

   -- N_Par_Path (Short)
   -- N_Full_Path (Short)
   --
   --   Get/Set_Specify_Input (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Specify_Output (Field3)
   --
   --   Get/Set_Path_Delay (Field4)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Polarity (State1)

   -- N_Path_Element (Short)
   --
   --   Get/Set_Lvalue (Field1)
   --
   --   Get/Set_Chain (Field2)

   -- N_Par_Edge_Path (Short)
   -- N_Full_Edge_Path (Short)
   --
   --   Get/Set_Specify_Input (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Specify_Output (Field3)
   --
   --   Get/Set_Path_Delay (Field4)
   --
   --   Get/Set_Data_Source (Field5)
   --
   --   Get/Set_Parent (Field6)
   --
   --   Get/Set_Polarity (State1)

   -- N_Path_Delay3 (Short)
   --
   --   Get/Set_Delay_Rise (Field1)
   --
   --   Get/Set_Delay_Fall (Field2)
   --
   --   Get/Set_Delay_Z (Field3)

   -- N_Path_Delay6 (Short)
   --
   --   Get/Set_Delay_01 (Field1)
   --
   --   Get/Set_Delay_10 (Field2)
   --
   --   Get/Set_Delay_0z (Field3)
   --
   --   Get/Set_Delay_z1 (Field4)
   --
   --   Get/Set_Delay_1z (Field5)
   --
   --   Get/Set_Delay_z0 (Field6)

   -- N_Path_Delay12 (Medium)
   --
   --   Get/Set_Delay_01 (Field1)
   --
   --   Get/Set_Delay_10 (Field2)
   --
   --   Get/Set_Delay_0z (Field3)
   --
   --   Get/Set_Delay_z1 (Field4)
   --
   --   Get/Set_Delay_1z (Field5)
   --
   --   Get/Set_Delay_z0 (Field6)
   --
   --   Get/Set_Delay_0x (Field7)
   --
   --   Get/Set_Delay_x1 (Field8)
   --
   --   Get/Set_Delay_1x (Field9)
   --
   --   Get/Set_Delay_x0 (Field10)
   --
   --   Get/Set_Delay_xz (Field11)
   --
   --   Get/Set_Delay_zx (Field12)

   -- N_Error (Short)

   -- N_Label (Short)
   --
   --   Get/Set_Label_Number (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label_Chain (Field3)
   --
   --   Get/Set_Label_Use (Field4)

   -- N_Goto (Short)
   --
   --   Get/Set_Label (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Suspend_Flag (Flag1)

   -- End of Nkind.

   Null_Node : constant Node := 0;
   Null_Vlg_Node : constant Node := 0;

   --  Get the number of the last node.
   --  To be used to size lateral tables.
   function Get_Last_Node return Node;

   -- General methods.

   function Create_Node (Kind : Nkind) return Node;
   procedure Free_Node (N : Node);

   --  Note: use Field0
   function Get_Location (N : Node) return Location_Type;
   procedure Set_Location (N : Node; Loc : Location_Type);

   function Get_Kind (N : Node) return Nkind;

   --  Mutate an N_Module_Instance to an N_Interface_Instance,
   --  N_Program_Instance.
   procedure Mutate_Instance (N : Node; Kind : Nkind);

   --  Mutate an Nkinds_Net_Ports to an N_Interface_Port
   procedure Mutate_Port (N : Node; Kind : Nkind);

   --  Mutate a N_Dotted_Name to N_Hierarchical
   procedure Mutate_Dotted_Name (N : Node; Kind : Nkind);

   --  Mutate a N_Name to N_Property_Name.
   procedure Mutate_Name (N : Node; Kind : Nkind);

   --  Field: Field6 Ref
   function Get_Parent (N : Node) return Node;
   procedure Set_Parent (N : Node; Parent : Node);

   --  Field: Field2 Ref
   function Get_Call_Scope (N : Node) return Node;
   procedure Set_Call_Scope (N : Node; Scope : Node);

   --  Field: Field1 (pos)
   function Get_Identifier (N : Node) return Name_Id;
   procedure Set_Identifier (N : Node; Id : Name_Id);

   --  For DPI routines.
   --  Field: Field4 (pos)
   function Get_C_Identifier (N : Node) return Name_Id;
   procedure Set_C_Identifier (N : Node; Id : Name_Id);

   --  Field: Field7 Chain
   function Get_Ports_Chain (N : Node) return Node;
   procedure Set_Ports_Chain (N : Node; Chain : Node);

   --  Field: Field7 Chain
   function Get_Tf_Ports_Chain (N : Node) return Node;
   procedure Set_Tf_Ports_Chain (N : Node; Chain : Node);

   --  Field: Field10 Chain
   function Get_Package_Import_Chain (N : Node) return Node;
   procedure Set_Package_Import_Chain (N : Node; Imp : Node);

   --  Field: Field3 Chain
   function Get_Parameter_Port_Chain (N : Node) return Node;
   procedure Set_Parameter_Port_Chain (N : Node; Port : Node);

   --  Field: Field5 Ref
   function Get_Parameter (N : Node) return Node;
   procedure Set_Parameter (N : Node; Decl : Node);

   --  Field: Field4 (uc)
   function Get_Foreign_Node (N : Node) return Int32;
   procedure Set_Foreign_Node (N : Node; Fn : Int32);

   --  Field: Field3 Chain
   function Get_Descriptions (N : Node) return Node;
   procedure Set_Descriptions (N : Node; Chain : Node);

   --  Field: Field7 Chain
   function Get_Class_Item_Chain (N : Node) return Node;
   procedure Set_Class_Item_Chain (N : Node; Items : Node);

   --  Field: Field5 Chain
   function Get_Package_Item_Chain (N : Node) return Node;
   procedure Set_Package_Item_Chain (N : Node; Items : Node);

   --  For module, programs, interface.
   --  Field: Field8 Chain
   function Get_Items_Chain (N : Node) return Node;
   procedure Set_Items_Chain (N : Node; Items : Node);

   --  Field: Field5 Chain
   function Get_Clocking_Item_Chain (N : Node) return Node;
   procedure Set_Clocking_Item_Chain (N : Node; Items : Node);

   --  Field: Field9 Chain
   function Get_Tf_Item_Declaration_Chain (N : Node) return Node;
   procedure Set_Tf_Item_Declaration_Chain (N : Node; Items : Node);

   --  Field: Field3 Chain
   function Get_Block_Item_Declaration_Chain (N : Node) return Node;
   procedure Set_Block_Item_Declaration_Chain (N : Node; Items : Node);

   --  Field: Field4 Chain
   function Get_Generate_Item_Chain (N : Node) return Node;
   procedure Set_Generate_Item_Chain (N : Node; Items : Node);

   --  Field: Field5 Chain
   function Get_Specify_Item_Chain (N : Node) return Node;
   procedure Set_Specify_Item_Chain (N : Node; Items : Node);

   --  Field: Field4 Chain
   function Get_Statements_Chain (N : Node) return Node;
   procedure Set_Statements_Chain (N : Node; Stmt : Node);

   --  Field: Field3 Chain
   function Get_Modport_Ports_Chain (N : Node) return Node;
   procedure Set_Modport_Ports_Chain (N : Node; Ports : Node);

   --  Field: Field2 Chain_Next
   function Get_Chain (N : Node) return Node;
   procedure Set_Chain (N : Node; Chain : Node);

   --  Field: Field5 Chain
   function Get_Constraint_Block_Chain (N : Node) return Node;
   procedure Set_Constraint_Block_Chain (N : Node; Chain : Node);

   --  Field: Field5 Chain
   function Get_Constraint_Set (N : Node) return Node;
   procedure Set_Constraint_Set (N : Node; Chain : Node);

   --  Field: Field10
   function Get_OOB_Prefix (N : Node) return Node;
   procedure Set_OOB_Prefix (N : Node; Prefix : Node);

   --  Field: Field10 Ref
   function Get_Out_Of_Block_Declaration (N : Node) return Node;
   procedure Set_Out_Of_Block_Declaration (N : Node; Decl : Node);

   --  Field: Field5 (uc)
   function Get_Generate_Index (N : Node) return Int32;
   procedure Set_Generate_Index (N : Node; Idx : Int32);

   --  Field: Field8
   function Get_Return_Variable (N : Node) return Node;
   procedure Set_Return_Variable (N : Node; Var : Node);

   --  Field: Field3 Ref
   function Get_Return_Variable_Ref (N : Node) return Node;
   procedure Set_Return_Variable_Ref (N : Node; Var : Node);

   --  Field: Field11
   function Get_This_Variable (N : Node) return Node;
   procedure Set_This_Variable (N : Node; Var : Node);

   --  Field: Field4
   function Get_Expression (N : Node) return Node;
   procedure Set_Expression (N : Node; Expr : Node);

   --  Field: Field4
   function Get_Reject_Limit (N : Node) return Node;
   procedure Set_Reject_Limit (N : Node; Expr : Node);

   --  Field: Field7
   function Get_Error_Limit (N : Node) return Node;
   procedure Set_Error_Limit (N : Node; Expr : Node);

   --  Field: Field4
   function Get_Sequence (N : Node) return Node;
   procedure Set_Sequence (N : Node; Seq : Node);

   --  Field: Field4
   function Get_Init_Expression (N : Node) return Node;
   procedure Set_Init_Expression (N : Node; Expr : Node);

   --  Field: Field5
   function Get_Size_Expression (N : Node) return Node;
   procedure Set_Size_Expression (N : Node; Expr : Node);

   --  Field: Field7 Ref
   function Get_Override_Stmt (N : Node) return Node;
   procedure Set_Override_Stmt (N : Node; Stmt : Node);

   --  Field: Field8 Ref
   function Get_Parameter_Expression (N : Node) return Node;
   procedure Set_Parameter_Expression (N : Node; Expr : Node);

   --  Field: Field4
   function Get_Parameter_Type (N : Node) return Node;
   procedure Set_Parameter_Type (N : Node; Typ : Node);

   --  Field: Field10 Chain
   function Get_Value_Range (N : Node) return Node;
   procedure Set_Value_Range (N : Node; Rng : Node);

   --  Field: Flag1
   function Get_Lsb_Include_Flag (N : Node) return Boolean;
   procedure Set_Lsb_Include_Flag (N : Node; Flag : Boolean);

   --  Field: Flag2
   function Get_Msb_Include_Flag (N : Node) return Boolean;
   procedure Set_Msb_Include_Flag (N : Node; Flag : Boolean);

   --  Field: Field3
   function Get_Range (N : Node) return Node;
   procedure Set_Range (N : Node; Rng : Node);

   --  Field: Field5
   function Get_Msb (N : Node) return Node;
   procedure Set_Msb (N : Node; Msb : Node);

   --  Field: Field6
   function Get_Lsb (N : Node) return Node;
   procedure Set_Lsb (N : Node; Lsb : Node);

   --  Field: Field5 (uc)
   function Get_Msb_Cst (N : Node) return Int32;
   procedure Set_Msb_Cst (N : Node; Msb : Int32);

   --  Field: Field6 (uc)
   function Get_Lsb_Cst (N : Node) return Int32;
   procedure Set_Lsb_Cst (N : Node; Lsb : Int32);

   --  Field: Field4
   function Get_Base_Expr (N : Node) return Node;
   procedure Set_Base_Expr (N : Node; Expr : Node);

   --  Field: Field5
   function Get_Width_Expr (N : Node) return Node;
   procedure Set_Width_Expr (N : Node; Expr : Node);

   --  Field: Field5 (uc)
   function Get_Width_Cst (N : Node) return Int32;
   procedure Set_Width_Cst (N : Node; Expr : Int32);

   --  Field: Field4 (uc)
   function Get_Type_Width (N : Node) return Width_Type;
   procedure Set_Type_Width (N : Node; Width : Width_Type);

   --  Field: Field4 (uc)
   function Get_Type_Size (N : Node) return Tsize_Type;
   procedure Set_Type_Size (N : Node; Width : Tsize_Type);

   --  Field: Field1 (uc)
   function Get_Stride_Width (N : Node) return Width_Type;
   procedure Set_Stride_Width (N : Node; Width : Width_Type);

   --  Field: Field1 (uc)
   function Get_Stride_Size (N : Node) return Tsize_Type;
   procedure Set_Stride_Size (N : Node; Width : Tsize_Type);

   --  Field: Field2 (uc)
   function Get_Type_Hash (N : Node) return Uns32;
   procedure Set_Type_Hash (N : Node; Width : Uns32);

   --  Field: Field1
   function Get_Maximum_Size_Expr (N : Node) return Node;
   procedure Set_Maximum_Size_Expr (N : Node; Size : Node);

   --  Field: Field1 (uc)
   function Get_Maximum_Size_Cst (N : Node) return Int32;
   procedure Set_Maximum_Size_Cst (N : Node; Size : Int32);

   --  Field: Field1
   function Get_Lvalue (N : Node) return Node;
   procedure Set_Lvalue (N : Node; Val : Node);

   --  Field: Field2
   function Get_Name (N : Node) return Node;
   procedure Set_Name (N : Node; Ref : Node);

   --  Field: Field5
   function Get_Item_Name (N : Node) return Node;
   procedure Set_Item_Name (N : Node; Ref : Node);

   --  Field: Field5
   function Get_Pattern_Key (N : Node) return Node;
   procedure Set_Pattern_Key (N : Node; Ref : Node);

   --  Field: Field1
   function Get_Left (N : Node) return Node;
   procedure Set_Left (N : Node; Val : Node);

   --  Field: Field4
   function Get_Right (N : Node) return Node;
   procedure Set_Right (N : Node; Val : Node);

   --  Field: Field5
   function Get_Repeat_Expression (N : Node) return Node;
   procedure Set_Repeat_Expression (N : Node; Expr : Node);

   --  Field: Field2 Chain
   function Get_Op_Attributes (N : Node) return Node;
   procedure Set_Op_Attributes (N : Node; Attrs : Node);

   --  Field: Field9 Chain
   function Get_Attributes_Chain (N : Node) return Node;
   procedure Set_Attributes_Chain (N : Node; Attrs : Node);

   --  Field: Field5
   function Get_Condition (N : Node) return Node;
   procedure Set_Condition (N : Node; Expr : Node);

   --  Field: Field1
   function Get_Cond_True (N : Node) return Node;
   procedure Set_Cond_True (N : Node; Expr : Node);

   --  Field: Field4
   function Get_Cond_False (N : Node) return Node;
   procedure Set_Cond_False (N : Node; Expr : Node);

   --  Field: Field3
   function Get_True_Stmt (N : Node) return Node;
   procedure Set_True_Stmt (N : Node; Stmt : Node);

   --  Field: Field4
   function Get_False_Stmt (N : Node) return Node;
   procedure Set_False_Stmt (N : Node; Stmt : Node);

   --  Field: Field4
   function Get_Pass_Stmt (N : Node) return Node;
   procedure Set_Pass_Stmt (N : Node; Stmt : Node);

   --  Field: Field3
   function Get_Else_Stmt (N : Node) return Node;
   procedure Set_Else_Stmt (N : Node; Stmt : Node);

   --  Field: Field9
   function Get_Clocking_Event (N : Node) return Node;
   procedure Set_Clocking_Event (N : Node; Ev : Node);

   --  Field: Field10
   function Get_Disable_Expression (N : Node) return Node;
   procedure Set_Disable_Expression (N : Node; Expr : Node);

   --  Field: Field11
   function Get_Property_Expression (N : Node) return Node;
   procedure Set_Property_Expression (N : Node; Expr : Node);

   --  Field: Field3
   function Get_True_Block (N : Node) return Node;
   procedure Set_True_Block (N : Node; Val : Node);

   --  Field: Field4
   function Get_False_Block (N : Node) return Node;
   procedure Set_False_Block (N : Node; Val : Node);

   --  Field: Field1
   function Get_Statement (N : Node) return Node;
   procedure Set_Statement (N : Node; Stmt : Node);

   --  Field: Field3
   function Get_Foreach_Array (N : Node) return Node;
   procedure Set_Foreach_Array (N : Node; Arr : Node);

   --  Field: Field4
   function Get_Foreach_Variables (N : Node) return Node;
   procedure Set_Foreach_Variables (N : Node; Vars : Node);

   --  Field: Field5
   function Get_Control (N : Node) return Node;
   procedure Set_Control (N : Node; Ctrl : Node);

   --  Field: Field1
   function Get_Replication (N : Node) return Node;
   procedure Set_Replication (N : Node; Expr : Node);

   --  Field: Field1 (uc)
   function Get_Replication_Cst (N : Node) return Int32;
   procedure Set_Replication_Cst (N : Node; Msb : Int32);

   --  Field: Field2 Chain
   function Get_Expressions (N : Node) return Node;
   procedure Set_Expressions (N : Node; Expr : Node);

   --  Field: Field2 Chain
   function Get_Elements (N : Node) return Node;
   procedure Set_Elements (N : Node; Els : Node);

   --  Field: Field4
   function Get_Slice_Size_Expr (N : Node) return Node;
   procedure Set_Slice_Size_Expr (N : Node; Expr : Node);

   --  Field: Field4 Maybe_Ref2
   function Get_Slice_Size_Type (N : Node) return Node;
   procedure Set_Slice_Size_Type (N : Node; Expr : Node);

   --  Field: Field1 Chain
   function Get_Members (N : Node) return Node;
   procedure Set_Members (N : Node; Els : Node);

   --  Field: Field2 (uc)
   function Get_Nbr_Members (N : Node) return Int32;
   procedure Set_Nbr_Members (N : Node; Nbr : Int32);

   --  Field: Field7 (uc)
   function Get_Member_Index (N : Node) return Int32;
   procedure Set_Member_Index (N : Node; Idx : Int32);

   --  Field: Field5 (uc)
   function Get_Packed_Member_Offset (N : Node) return Uns32;
   procedure Set_Packed_Member_Offset (N : Node; Idx : Uns32);

   --  Field: Field4 Chain
   function Get_Nature_Items (N : Node) return Node;
   procedure Set_Nature_Items (N : Node; Items : Node);

   --  Field: Field4 Chain
   function Get_Discipline_Items (N : Node) return Node;
   procedure Set_Discipline_Items (N : Node; Items : Node);

   --  Field: Flag1
   function Get_Continuous_Flag (N : Node) return Boolean;
   procedure Set_Continuous_Flag (N : Node; Flag : Boolean);

   --  Field: Flag1
   function Get_Potential_Flag (N : Node) return Boolean;
   procedure Set_Potential_Flag (N : Node; Flag : Boolean);

   --  Field: Field4
   function Get_Nature (N : Node) return Node;
   procedure Set_Nature (N : Node; Nature : Node);

   --  Field: Field8 Chain
   function Get_Connections (N : Node) return Node;
   procedure Set_Connections (N : Node; Conns : Node);

   --  Field: Field8 Chain
   function Get_Gate_Terminals (N : Node) return Node;
   procedure Set_Gate_Terminals (N : Node; Terms : Node);

   --  Field: Field9 Chain
   function Get_Parameter_Values (N : Node) return Node;
   procedure Set_Parameter_Values (N : Node; Values : Node);

   --  Field: Field1 Chain
   function Get_Case_Items (N : Node) return Node;
   procedure Set_Case_Items (N : Node; Items : Node);

   --  Field: Field6
   function Get_Delay (N : Node) return Node;
   procedure Set_Delay (N : Node; Value : Node);

   --  Field: Field8
   function Get_Net_Delay (N : Node) return Node;
   procedure Set_Net_Delay (N : Node; Value : Node);

   --  Field: Field4
   function Get_Gate_Delay (N : Node) return Node;
   procedure Set_Gate_Delay (N : Node; Value : Node);

   --  Field: Field3
   function Get_Assign_Delay (N : Node) return Node;
   procedure Set_Assign_Delay (N : Node; Value : Node);

   --  Field: Field1
   function Get_Rising_Delay (N : Node) return Node;
   procedure Set_Rising_Delay (N : Node; Value : Node);

   --  Field: Field2
   function Get_Falling_Delay (N : Node) return Node;
   procedure Set_Falling_Delay (N : Node; Value : Node);

   --  Field: Field3
   function Get_Highz_Delay (N : Node) return Node;
   procedure Set_Highz_Delay (N : Node; Value : Node);

   --  Field: Field3
   function Get_For_Initialization (N : Node) return Node;
   procedure Set_For_Initialization (N : Node; Assign : Node);

   --  Field: Field4
   function Get_Step_Assign (N : Node) return Node;
   procedure Set_Step_Assign (N : Node; Assign : Node);

   --  Field: Field4 Chain
   function Get_Arguments (N : Node) return Node;
   procedure Set_Arguments (N : Node; Args : Node);

   --  Field: Field5
   function Get_Iterator_Argument (N : Node) return Node;
   procedure Set_Iterator_Argument (N : Node; Arg : Node);

   --  Field: Field1
   function Get_Task (N : Node) return Node;
   procedure Set_Task (N : Node; Args : Node);

   --  Set when the number is signed.
   --  Field: Flag1
   function Get_Signed_Flag (N : Node) return Boolean;
   procedure Set_Signed_Flag (N : Node; Is_Signed : Boolean);

   --  Field: Flag1
   function Get_Scope_Flag (N : Node) return Boolean;
   procedure Set_Scope_Flag (N : Node; Scope : Boolean);

   --  Base of the number.
   --  Used only for informationnal purpose, such as displaying.
   --  Field: State1 (pos)
   function Get_Number_Base (N : Node) return Base_Type;
   procedure Set_Number_Base (N : Node; B : Base_Type);

   --  Field: Field1 (uc)
   function Get_Number_Hi_Val (N : Node) return Uns32;
   procedure Set_Number_Hi_Val (N : Node; Val : Uns32);

   --  Field: Field2 (uc)
   function Get_Number_Lo_Val (N : Node) return Uns32;
   procedure Set_Number_Lo_Val (N : Node; Val : Uns32);

   --  Field: Field4 (uc)
   function Get_Number_Hi_Zx (N : Node) return Uns32;
   procedure Set_Number_Hi_Zx (N : Node; Zx : Uns32);

   --  Field: Field5 (uc)
   function Get_Number_Lo_Zx (N : Node) return Uns32;
   procedure Set_Number_Lo_Zx (N : Node; Zx : Uns32);

   --  Field: Field6 (uc)
   function Get_Number_Size (N : Node) return Width_Type;
   procedure Set_Number_Size (N : Node; Bn : Width_Type);

   --  The expression that was evaluated to the number.
   --  Field: Field6
   function Get_Expr_Origin (N : Node) return Node;
   procedure Set_Expr_Origin (N : Node; Orig : Node);

   --  Index (of the least significant word) in verilog.bn_tables
   --  Field: Field1 (uc)
   function Get_Bignum_Index (N : Node) return Bn_Index;
   procedure Set_Bignum_Index (N : Node; Idx : Bn_Index);

   --  Number of bits
   --  Field: Field2 (uc)
   function Get_Bignum_Len (N : Node) return Uns32;
   procedure Set_Bignum_Len (N : Node; Len : Uns32);

   --  Field: Field1,Field2 (grp)
   function Get_Real_Number (N : Node) return Fp64;
   procedure Set_Real_Number (N : Node; Val : Fp64);

   --  Field: Field4 (uc)
   function Get_Time_Unit (N : Node) return Int32;
   procedure Set_Time_Unit (N : Node; Val : Int32);

   --  Field: Field4 (uc)
   function Get_Scale_Factor (N : Node) return Int32;
   procedure Set_Scale_Factor (N : Node; Val : Int32);

   --  Field: Field3 (uc)
   function Get_Time_Precision (N : Node) return Int32;
   procedure Set_Time_Precision (N : Node; Val : Int32);

   --  Field: Field5 Ref
   function Get_Timescale (N : Node) return Node;
   procedure Set_Timescale (N : Node; V : Node);

   --  Field: Field6 (uc)
   function Get_String_Size (N : Node) return Uns32;
   procedure Set_String_Size (N : Node; Bn : Uns32);

   --  The node belongs to N only if Get_Type_Onwer is True.  So ownership is
   --  handled by node rather than globally.
   --  Field: Field3 Maybe_Ref
   function Get_Data_Type (N : Node) return Node;
   procedure Set_Data_Type (N : Node; Atype : Node);

   --  Type of an expression or a name.  Never owned.
   --  Field: Field3 Ref
   function Get_Expr_Type (N : Node) return Node;
   procedure Set_Expr_Type (N : Node; Atype : Node);

   --  Field: Field9 Ref
   function Get_Param_Type (N : Node) return Node;
   procedure Set_Param_Type (N : Node; Atype : Node);

   --  Like Set_Type: ownership is defined by Get_Type_Owner.
   --  Field: Field2 Maybe_Ref
   function Get_Element_Data_Type (N : Node) return Node;
   procedure Set_Element_Data_Type (N : Node; El : Node);

   --  Field: Field2 Ref
   function Get_Type_Element_Type (N : Node) return Node;
   procedure Set_Type_Element_Type (N : Node; El : Node);

   --  Field: Field2 Maybe_Ref
   function Get_Cast_Data_Type (N : Node) return Node;
   procedure Set_Cast_Data_Type (N : Node; Atype : Node);

   --  Like Set_Data_Type: ownership is defined by Get_Type_Owner.
   --  Field: Field4 Maybe_Ref
   function Get_Base_Class_Type (N : Node) return Node;
   procedure Set_Base_Class_Type (N : Node; El : Node);

   --  Field: Field8 Forward_Ref
   function Get_Class_Constructor (N : Node) return Node;
   procedure Set_Class_Constructor (N : Node; El : Node);

   --  Field: Field9 (uc)
   function Get_Inheritance_Depth (N : Node) return Int32;
   procedure Set_Inheritance_Depth (N : Node; D : Int32);

   --  Like Set_Type: ownership is defined by Get_Type_Owner.
   --  Field: Field5 Maybe_Ref
   function Get_Enum_Base_Data_Type (N : Node) return Node;
   procedure Set_Enum_Base_Data_Type (N : Node; Typ : Node);

   --  Field: Field2 Ref
   function Get_Enum_Base_Type (N : Node) return Node;
   procedure Set_Enum_Base_Type (N : Node; Typ : Node);

   --  Field: Field5 Ref
   function Get_Packed_Base_Type (N : Node) return Node;
   procedure Set_Packed_Base_Type (N : Node; Typ : Node);

   --  Like Set_Type: ownership is defined by Get_Type_Owner.
   --  Field: Field3 Maybe_Ref
   function Get_Default_Type (N : Node) return Node;
   procedure Set_Default_Type (N : Node; Typ : Node);

   --  True iff node N owns its type.
   --  Field: Flag3
   function Get_Type_Owner (N : Node) return Boolean;
   procedure Set_Type_Owner (N : Node; Flag : Boolean);

   --  Field: Flag5
   function Get_Type_Owner_2 (N : Node) return Boolean;
   procedure Set_Type_Owner_2 (N : Node; Flag : Boolean);

   --  Note: a forward ref.
   --  Field: Field3 Ref
   function Get_Forward_Type (N : Node) return Node;
   procedure Set_Forward_Type (N : Node; Atype : Node);

   --  Field: Field1 Chain
   function Get_Enum_Names (N : Node) return Node;
   procedure Set_Enum_Names (N : Node; El : Node);

   --  Ownership is given by Type_Owner_2
   --  Field: Field1 Maybe_Ref2
   function Get_Index_Data_Type (N : Node) return Node;
   procedure Set_Index_Data_Type (N : Node; El : Node);

   --  Field: Field1 Ref
   function Get_Type_Index_Type (N : Node) return Node;
   procedure Set_Type_Index_Type (N : Node; El : Node);

   --  Ownership is given by Type_Owner
   --  Field: Field1 Maybe_Ref
   function Get_Type_Argument (N : Node) return Node;
   procedure Set_Type_Argument (N : Node; El : Node);

   --  True if the type is signed.
   --  Field: Flag1
   function Get_Type_Signed (N : Node) return Boolean;
   procedure Set_Type_Signed (N : Node; Flag : Boolean);

   --  Field: Field1
   function Get_Subroutine (N : Node) return Node;
   procedure Set_Subroutine (N : Node; Sub : Node);

   --  Field: Field2
   function Get_Object (N : Node) return Node;
   procedure Set_Object (N : Node; Obj : Node);

   --  Field: Field6
   function Get_With_Expression (N : Node) return Node;
   procedure Set_With_Expression (N : Node; Expr : Node);

   --  Field: Field5 (uc)
   function Get_Drive_Strength (N : Node) return Int32;
   procedure Set_Drive_Strength (N : Node; Strength : Int32);

   --  Field: Field9 (uc)
   function Get_Net_Drive_Strength (N : Node) return Int32;
   procedure Set_Net_Drive_Strength (N : Node; Strength : Int32);

   --  Field: Field9 (uc)
   function Get_Charge_Strength (N : Node) return Int32;
   procedure Set_Charge_Strength (N : Node; Strength : Int32);

   --  Name for the module.  This is only a reference.
   --  Field: Field7
   function Get_Module (N : Node) return Node;
   procedure Set_Module (N : Node; M : Node);

   --  Field: Field5
   function Get_Class_Name (N : Node) return Node;
   procedure Set_Class_Name (N : Node; C : Node);

   --  Field: Field3
   function Get_Interface (N : Node) return Node;
   procedure Set_Interface (N : Node; C : Node);

   --  Field: Field7
   function Get_Interface_Name (N : Node) return Node;
   procedure Set_Interface_Name (N : Node; Name : Node);

   --  A copy of the module that is the instance.
   --  Field: Field4
   function Get_Instance (N : Node) return Node;
   procedure Set_Instance (N : Node; Inst : Node);

   --  Field: Field4 Ref
   function Get_Instance_Ref (N : Node) return Node;
   procedure Set_Instance_Ref (N : Node; Inst : Node);

   --  Field: Field3 Ref
   function Get_Port (N : Node) return Node;
   procedure Set_Port (N : Node; Port : Node);

   --  Field: Flag1
   function Get_Collapse_Flag (N : Node) return Boolean;
   procedure Set_Collapse_Flag (N : Node; Val : Boolean);

   --  Field: Field1 (pos)
   function Get_Unary_Op (N : Node) return Unary_Ops;
   procedure Set_Unary_Op (N : Node; Op : Unary_Ops);

   --  Field: Field5 (pos)
   function Get_Binary_Op (N : Node) return Binary_Ops;
   procedure Set_Binary_Op (N : Node; Op : Binary_Ops);

   --  Field: Field1 (pos)
   function Get_Conversion_Op (N : Node) return Conv_Ops;
   procedure Set_Conversion_Op (N : Node; Op : Conv_Ops);

   --  Field: Field4 Ref
   function Get_Declaration (N : Node) return Node;
   procedure Set_Declaration (N : Node; Decl : Node);

   --  Field: Field4 Forward_Ref
   function Get_Redeclaration (N : Node) return Node;
   procedure Set_Redeclaration (N : Node; Decl : Node);

   --  Field: Field2 Ref
   function Get_This_Declaration (N : Node) return Node;
   procedure Set_This_Declaration (N : Node; Decl : Node);

   --  Field: Field7
   function Get_Default_Value (N : Node) return Node;
   procedure Set_Default_Value (N : Node; Expr : Node);

   --  Field: Flag1
   function Get_Instantiated_Flag (N : Node) return Boolean;
   procedure Set_Instantiated_Flag (N : Node; Flag : Boolean);

   --  If set, the module is using port declarations, aka ANSI ports.
   --  If not set, the module uses a port_list and the ports are declared
   --   in the items list.
   --  Field: Flag4
   function Get_Ansi_Port_Flag (N : Node) return Boolean;
   procedure Set_Ansi_Port_Flag (N : Node; Flag : Boolean);

   --  Field: Field4
   function Get_Event (N : Node) return Node;
   procedure Set_Event (N : Node; Event : Node);

   --  Field: Field1
   function Get_Min_Expr (N : Node) return Node;
   procedure Set_Min_Expr (N : Node; Expr : Node);

   --  Field: Field2
   function Get_Typ_Expr (N : Node) return Node;
   procedure Set_Typ_Expr (N : Node; Expr : Node);

   --  Field: Field4
   function Get_Max_Expr (N : Node) return Node;
   procedure Set_Max_Expr (N : Node; Expr : Node);

   --  Field: Field3 Chain
   function Get_Udp_Port_Declaration_Chain (N : Node) return Node;
   procedure Set_Udp_Port_Declaration_Chain (N : Node; Chain : Node);

   --  Field: State1 (pos)
   function Get_Udp_Kind (N : Node) return Udp_Kind;
   procedure Set_Udp_Kind (N : Node; K : Udp_Kind);

   --  Field: Field4
   function Get_Udp_Initial (N : Node) return Node;
   procedure Set_Udp_Initial (N : Node; Expr : Node);

   --  Field: Field5 Chain
   function Get_Udp_Entries_Chain (N : Node) return Node;
   procedure Set_Udp_Entries_Chain (N : Node; C : Node);

   --  Field: Field1 Chain
   function Get_Input_Chain (N : Node) return Node;
   procedure Set_Input_Chain (N : Node; C : Node);

   --  Field: Field3 (pos)
   function Get_Output_Symbol (N : Node) return Udp_Symbol;
   procedure Set_Output_Symbol (N : Node; S : Udp_Symbol);

   --  Field: Field3 (pos)
   function Get_Current_State (N : Node) return Udp_Symbol;
   procedure Set_Current_State (N : Node; S : Udp_Symbol);

   --  Field: Field4 (pos)
   function Get_Next_State (N : Node) return Udp_Symbol;
   procedure Set_Next_State (N : Node; S : Udp_Symbol);

   --  Field: Field1 (pos)
   function Get_Symbol (N : Node) return Udp_Symbol;
   procedure Set_Symbol (N : Node; S : Udp_Symbol);

   --  Field: Field1 (pos)
   function Get_From_Symbol (N : Node) return Udp_Symbol;
   procedure Set_From_Symbol (N : Node; S : Udp_Symbol);

   --  Field: Field3 (pos)
   function Get_To_Symbol (N : Node) return Udp_Symbol;
   procedure Set_To_Symbol (N : Node; S : Udp_Symbol);

   --  Field: Field1
   function Get_Specify_Input (N : Node) return Node;
   procedure Set_Specify_Input (N : Node; Input : Node);

   --  Field: Field3
   function Get_Specify_Output (N : Node) return Node;
   procedure Set_Specify_Output (N : Node; Output : Node);

   --  Field: Field4
   function Get_Path_Delay (N : Node) return Node;
   procedure Set_Path_Delay (N : Node; Dly : Node);

   --  Field: Field5
   function Get_Data_Source (N : Node) return Node;
   procedure Set_Data_Source (N : Node; Data : Node);

   --  Field: State1 (pos)
   function Get_Polarity (N : Node) return Polarity_Type;
   procedure Set_Polarity (N : Node; Polarity : Polarity_Type);

   --  Field: Field1
   function Get_Delay_Rise (N : Node) return Node;
   procedure Set_Delay_Rise (N : Node; Dly : Node);

   --  Field: Field2
   function Get_Delay_Fall (N : Node) return Node;
   procedure Set_Delay_Fall (N : Node; Dly : Node);

   --  Field: Field3
   function Get_Delay_Z (N : Node) return Node;
   procedure Set_Delay_Z (N : Node; Dly : Node);

   --  Field: Field1
   function Get_Delay_01 (N : Node) return Node;
   procedure Set_Delay_01 (N : Node; Dly : Node);

   --  Field: Field2
   function Get_Delay_10 (N : Node) return Node;
   procedure Set_Delay_10 (N : Node; Dly : Node);

   --  Field: Field3
   function Get_Delay_0z (N : Node) return Node;
   procedure Set_Delay_0z (N : Node; Dly : Node);

   --  Field: Field4
   function Get_Delay_z1 (N : Node) return Node;
   procedure Set_Delay_z1 (N : Node; Dly : Node);

   --  Field: Field5
   function Get_Delay_1z (N : Node) return Node;
   procedure Set_Delay_1z (N : Node; Dly : Node);

   --  Field: Field6
   function Get_Delay_z0 (N : Node) return Node;
   procedure Set_Delay_z0 (N : Node; Dly : Node);

   --  Field: Field7
   function Get_Delay_0x (N : Node) return Node;
   procedure Set_Delay_0x (N : Node; Dly : Node);

   --  Field: Field8
   function Get_Delay_x1 (N : Node) return Node;
   procedure Set_Delay_x1 (N : Node; Dly : Node);

   --  Field: Field9
   function Get_Delay_1x (N : Node) return Node;
   procedure Set_Delay_1x (N : Node; Dly : Node);

   --  Field: Field10
   function Get_Delay_x0 (N : Node) return Node;
   procedure Set_Delay_x0 (N : Node; Dly : Node);

   --  Field: Field11
   function Get_Delay_xz (N : Node) return Node;
   procedure Set_Delay_xz (N : Node; Dly : Node);

   --  Field: Field12
   function Get_Delay_zx (N : Node) return Node;
   procedure Set_Delay_zx (N : Node; Dly : Node);

   --  Field: Field1 (pos)
   function Get_String_Id (N : Node) return String8_Id;
   procedure Set_String_Id (N : Node; Id : String8_Id);

   --  Field: Field1
   function Get_Label (N : Node) return Node;
   procedure Set_Label (N : Node; L : Node);

   --  Field: Field1 (pos)
   function Get_Label_Number (N : Node) return Int32;
   procedure Set_Label_Number (N : Node; Val : Int32);

   --  Field: Field3
   function Get_Label_Chain (N : Node) return Node;
   procedure Set_Label_Chain (N : Node; Chain : Node);

   --  Number of gotos to this label.
   --  Field: Field4 (pos)
   function Get_Label_Use (N : Node) return Int32;
   procedure Set_Label_Use (N : Node; Val : Int32);

   --  If true, the goto must go through the simulator kernel (because
   --   execution must be suspended).
   --  Field: Flag1
   function Get_Suspend_Flag (N : Node) return Boolean;
   procedure Set_Suspend_Flag (N : Node; Flag : Boolean);

   --  Field: Flag1
   function Get_Same_Case_Flag (N : Node) return Boolean;
   procedure Set_Same_Case_Flag (N : Node; Flag : Boolean);

   --  Field: Field5 (pos)
   function Get_Obj_Id (N : Node) return Obj_Id;
   procedure Set_Obj_Id (N : Node; Id : Obj_Id);

   --  Field: Field5 (pos)
   function Get_Scope_Id (N : Node) return Scope_Id;
   procedure Set_Scope_Id (N : Node; Id : Scope_Id);

   --  Field: Field5 (pos)
   function Get_Process_Id (N : Node) return Proc_Id;
   procedure Set_Process_Id (N : Node; Id : Proc_Id);

   --  Field: Field5 (pos)
   function Get_Sys_Tf_Id (N : Node) return Sys_Tf_Id;
   procedure Set_Sys_Tf_Id (N : Node; Id : Sys_Tf_Id);

   --  Field: Field5 (pos)
   function Get_Lit_Id (N : Node) return Lit_Id;
   procedure Set_Lit_Id (N : Node; Id : Lit_Id);

   --  Field: Field1
   function Get_Generate_Block (N : Node) return Node;
   procedure Set_Generate_Block (N : Node; Conn : Node);

   --  Field: Field3
   function Get_Input_Skew (N : Node) return Node;
   procedure Set_Input_Skew (N : Node; S : Node);

   --  Field: Field5
   function Get_Output_Skew (N : Node) return Node;
   procedure Set_Output_Skew (N : Node; S : Node);

   --  Field: Field1
   function Get_Delay_Control (N : Node) return Node;
   procedure Set_Delay_Control (N : Node; D : Node);

   --  Field: Field3 Forward_Ref
   function Get_Attribute_Item (N : Node) return Node;
   procedure Set_Attribute_Item (N : Node; Item : Node);

   --  Layout flag for object declaration.  If True, the identifier of this
   --  declaration is followed by an identifier (and separated by a comma).
   --  This flag is set on all but the last declarations.
   --  Eg: on 'wire [1:0] a, b, c;', the flag is set on a and b (but not c).
   --  Field: Flag1
   function Get_Has_Identifier_List (Decl : Node) return Boolean;
   procedure Set_Has_Identifier_List (Decl : Node; Flag : Boolean);

   --  Field: Flag4
   function Get_Has_Sign (Decl : Node) return Boolean;
   procedure Set_Has_Sign (Decl : Node; Flag : Boolean);

   --  Field: Flag4
   function Get_Connected_Flag (N : Node) return Boolean;
   procedure Set_Connected_Flag (N : Node; Flag : Boolean);

   --  Field: Flag2
   function Get_Complete_Flag (N : Node) return Boolean;
   procedure Set_Complete_Flag (N : Node; Flag : Boolean);

   --  This flag is set for nets implicitly created by assign statements, by
   --  module instantiation or by a port.
   --  Field: Flag2
   function Get_Implicit_Flag (N : Node) return Boolean;
   procedure Set_Implicit_Flag (N : Node; Flag : Boolean);

   --  This flag is set for nets that redeclare a port.
   --  Field: Flag5
   function Get_Redeclaration_Flag (N : Node) return Boolean;
   procedure Set_Redeclaration_Flag (N : Node; Flag : Boolean);

   --  Set if the object is allocated on the frame.  A variable can be declared
   --  as automatic but still statically allocated.
   --  Field: Flag10
   function Get_Is_Automatic (N : Node) return Boolean;
   procedure Set_Is_Automatic (N : Node; Flag : Boolean);

   --  For object: lifetime
   --  For statements: default lifetime of objects declared.
   --  The lifetime determines how an object is initialized.  For placement,
   --  Is_Automatic must be used.
   --  Field: Flag6 (uc)
   function Get_Lifetime (N : Node) return Lifetime_Type;
   procedure Set_Lifetime (N : Node; Live : Lifetime_Type);

   --  True if static/automatic has been specified by the user.
   --  Field: Flag7
   function Get_Has_Lifetime (N : Node) return Boolean;
   procedure Set_Has_Lifetime (N : Node; Flag : Boolean);

   --  Field: Flag5
   function Get_Has_End_Name (N : Node) return Boolean;
   procedure Set_Has_End_Name (N : Node; Flag : Boolean);

   --  Field: Field1
   function Get_Call (N : Node) return Node;
   procedure Set_Call (N : Node; Call : Node);

   --  Field: Field3
   function Get_Timeunit (N : Node) return Node;
   procedure Set_Timeunit (N : Node; Time : Node);

   --  Field: Field4
   function Get_Timeprecision (N : Node) return Node;
   procedure Set_Timeprecision (N : Node; Time : Node);

   --  Field: Field1
   function Get_Error_Origin (N : Node) return Node;
   procedure Set_Error_Origin (N : Node; Orig : Node);

   --  Field: Flag1
   function Get_Has_Void_Cast (N : Node) return Boolean;
   procedure Set_Has_Void_Cast (N : Node; Flag : Boolean);

   --  Field: Flag2
   function Get_Is_Const (N : Node) return Boolean;
   procedure Set_Is_Const (N : Node; Flag : Boolean);

   --  Field: Flag4
   function Get_Has_Var (N : Node) return Boolean;
   procedure Set_Has_Var (N : Node; Flag : Boolean);

   --  Field: Flag4
   function Get_Has_Type (N : Node) return Boolean;
   procedure Set_Has_Type (N : Node; Flag : Boolean);

   --  Field: Flag5
   function Get_Has_Direction (N : Node) return Boolean;
   procedure Set_Has_Direction (N : Node; Flag : Boolean);

   --  Field: Flag1
   function Get_Has_Parenthesis (N : Node) return Boolean;
   procedure Set_Has_Parenthesis (N : Node; Flag : Boolean);

   --  Field: Flag2
   function Get_Has_Argument (N : Node) return Boolean;
   procedure Set_Has_Argument (N : Node; Flag : Boolean);

   --  Set on type/subroutine/declaration when fully analyzed.
   --  Field: Flag8
   function Get_Fully_Analyzed_Flag (N : Node) return Boolean;
   procedure Set_Fully_Analyzed_Flag (N : Node; Flag : Boolean);

   --  Field: Flag8
   function Get_Resolved_Flag (N : Node) return Boolean;
   procedure Set_Resolved_Flag (N : Node; Flag : Boolean);

   --  General purpose flag, used during a walk.
   --  Field: Flag9
   function Get_Mark_Flag (N : Node) return Boolean;
   procedure Set_Mark_Flag (N : Node; Flag : Boolean);

   --  True if the expression is constant
   --  Field: Flag4
   function Get_Is_Constant (N : Node) return Boolean;
   procedure Set_Is_Constant (N : Node; Flag : Boolean);

   --  Field: Flag14
   function Get_Static_Flag (N : Node) return Boolean;
   procedure Set_Static_Flag (N : Node; Flag : Boolean);

   --  Field: Flag19
   function Get_Has_Attribute (N : Node) return Boolean;
   procedure Set_Has_Attribute (N : Node; Flag : Boolean);

   --  Field: Flag1
   function Get_Attribute_Full (N : Node) return Boolean;
   procedure Set_Attribute_Full (N : Node; Flag : Boolean);

   --  Field: Flag2
   function Get_Attribute_Parallel (N : Node) return Boolean;
   procedure Set_Attribute_Parallel (N : Node; Flag : Boolean);

   --  Field: Flag3
   function Get_Other_Attributes (N : Node) return Boolean;
   procedure Set_Other_Attributes (N : Node; Flag : Boolean);

   --  Field: Flag1
   function Get_Pure_Property (N : Node) return Boolean;
   procedure Set_Pure_Property (N : Node; P : Boolean);

   --  Field: Flag2
   function Get_Context_Property (N : Node) return Boolean;
   procedure Set_Context_Property (N : Node; P : Boolean);

   --  Field: Flag11
   function Get_Has_Extern_Flag (N : Node) return Boolean;
   procedure Set_Has_Extern_Flag (N : Node; Flag : Boolean);

   --  Field: Flag12
   function Get_Virtual_Flag (N : Node) return Boolean;
   procedure Set_Virtual_Flag (N : Node; Flag : Boolean);

   --  Field: Flag1
   function Get_Pure_Flag (N : Node) return Boolean;
   procedure Set_Pure_Flag (N : Node; Flag : Boolean);

   --  Field: State1 (pos)
   function Get_Join_Option (N : Node) return Join_Type;
   procedure Set_Join_Option (N : Node; Opt : Join_Type);

   --  Field: State1 (pos)
   function Get_Edge_Identifier (N : Node) return Edge_Type;
   procedure Set_Edge_Identifier (N : Node; Edge : Edge_Type);

   --  Field: State1 (pos)
   function Get_DPI_Spec (N : Node) return DPI_Spec_Type;
   procedure Set_DPI_Spec (N : Node; Spec : DPI_Spec_Type);

   --  Field: State1 (pos)
   function Get_Visibility (N : Node) return Visibility_Type;
   procedure Set_Visibility (N : Node; Vis : Visibility_Type);

   --  Field: State1 (pos)
   function Get_Class_Visibility (N : Node) return Visibility_Type;
   procedure Set_Class_Visibility (N : Node; Vis : Visibility_Type);

   --  Field: Flag11
   function Get_Has_Visibility (N : Node) return Boolean;
   procedure Set_Has_Visibility (N : Node; F : Boolean);

   --  Field: State1 (pos)
   function Get_Violation (N : Node) return Violation_Type;
   procedure Set_Violation (N : Node; V : Violation_Type);

   --  Field: Flag12
   function Get_Random_Flag (N : Node) return Boolean;
   procedure Set_Random_Flag (N : Node; F : Boolean);

   --  Field: Flag13
   function Get_Randc_Flag (N : Node) return Boolean;
   procedure Set_Randc_Flag (N : Node; F : Boolean);

   --  Set when the size has been computed.
   --  Field: Flag2
   function Get_Size_Flag (N : Node) return Boolean;
   procedure Set_Size_Flag (N : Node; F : Boolean);

   --  Field: Flag1
   function Get_Type_Analyzed_Flag (N : Node) return Boolean;
   procedure Set_Type_Analyzed_Flag (N : Node; F : Boolean);

   --  True if there is a forward typedef for N.  False if this is the first
   --  declaration of the type.
   --  Field: Flag4
   function Get_Forward_Typedef_Flag (N : Node) return Boolean;
   procedure Set_Forward_Typedef_Flag (N : Node; F : Boolean);

   --  Field: Field1
   function Get_Access (N : Node) return Node;
   procedure Set_Access (N : Node; Acc : Node);

   --  Field: Field4
   function Get_Arg1 (N : Node) return Node;
   procedure Set_Arg1 (N : Node; Arg : Node);

   --  Field: Field5
   function Get_Arg2 (N : Node) return Node;
   procedure Set_Arg2 (N : Node; Arg : Node);
end Verilog.Nodes;
