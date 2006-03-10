with System; use System;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Hex_Images; use Hex_Images;

package body Disa_Sparc is
   subtype Reg_Type is Unsigned_32 range 0 .. 31;

   type Hex_Map_Type is array (Unsigned_32 range 0 .. 15) of Character;
   Hex_Digit : constant Hex_Map_Type := "0123456789abcdef";

   type Cstring_Acc is access constant String;
   type Cond_Map_Type is array (Unsigned_32 range 0 .. 15) of Cstring_Acc;
   subtype S is String;
   Bicc_Map : constant Cond_Map_Type :=
     (0 => new S'("n"),
      1 => new S'("e"),
      2 => new S'("le"),
      3 => new S'("l"),
      4 => new S'("leu"),
      5 => new S'("cs"),
      6 => new S'("neg"),
      7 => new S'("vs"),
      8 => new S'("a"),
      9 => new S'("ne"),
      10 => new S'("g"),
      11 => new S'("ge"),
      12 => new S'("gu"),
      13 => new S'("cc"),
      14 => new S'("pos"),
      15 => new S'("vc")
      );


   type Format_Type is
      (
       Format_Bad,
       Format_Regimm, --  format 3, rd, rs1, rs2 or imm13
       Format_Rd,     --  format 3, rd only.
       Format_Copro,  --  format 3, fpu or coprocessor
       Format_Asi     --  format 3, rd, rs1, asi and rs2.
       );

   type Insn_Desc_Type is record
      Name : Cstring_Acc;
      Format : Format_Type;
   end record;

   type Insn_Desc_Array is array (Unsigned_32 range 0 .. 63) of Insn_Desc_Type;
   Insn_Desc_10 : constant Insn_Desc_Array :=
     (
      2#000_000# => (new S'("add"), Format_Regimm),
      2#000_001# => (new S'("and"), Format_Regimm),
      2#000_010# => (new S'("or"), Format_Regimm),
      2#000_011# => (new S'("xor"), Format_Regimm),
      2#000_100# => (new S'("sub"), Format_Regimm),
      2#000_101# => (new S'("andn"), Format_Regimm),
      2#000_110# => (new S'("orn"), Format_Regimm),
      2#000_111# => (new S'("xnor"), Format_Regimm),
      2#001_000# => (new S'("addx"), Format_Regimm),

      2#001_100# => (new S'("subx"), Format_Regimm),

      2#010_000# => (new S'("addcc"), Format_Regimm),
      2#010_001# => (new S'("andcc"), Format_Regimm),
      2#010_010# => (new S'("orcc"), Format_Regimm),
      2#010_011# => (new S'("xorcc"), Format_Regimm),
      2#010_100# => (new S'("subcc"), Format_Regimm),
      2#010_101# => (new S'("andncc"), Format_Regimm),
      2#010_110# => (new S'("orncc"), Format_Regimm),
      2#010_111# => (new S'("xnorcc"), Format_Regimm),
      2#011_000# => (new S'("addxcc"), Format_Regimm),

      2#011_100# => (new S'("subxcc"), Format_Regimm),

      2#111_000# => (new S'("jmpl"), Format_Regimm),

      2#111_100# => (new S'("save"), Format_Regimm),
      2#111_101# => (new S'("restore"), Format_Regimm),

      others => (null, Format_Bad)
      );

   Insn_Desc_11 : constant Insn_Desc_Array :=
     (
      2#000_000# => (new S'("ld"), Format_Regimm),
      2#000_001# => (new S'("ldub"), Format_Regimm),
      2#000_010# => (new S'("lduh"), Format_Regimm),
      2#000_011# => (new S'("ldd"), Format_Regimm),
      2#000_100# => (new S'("st"), Format_Regimm),
      2#000_101# => (new S'("stb"), Format_Regimm),

      2#010_000# => (new S'("lda"), Format_Asi),
      2#010_011# => (new S'("ldda"), Format_Asi),

      2#110_000# => (new S'("ldc"), Format_Regimm),
      2#110_001# => (new S'("ldcsr"), Format_Regimm),

      others => (null, Format_Bad)
      );

   --  Disassemble instruction at ADDR, and put the result in LINE/LINE_LEN.
   procedure Disassemble_Insn (Addr : Address;
                               Line : in out String;
                               Line_Len : out Natural;
                               Insn_Len : out Natural;
                               Proc_Cb : Symbol_Proc_Type)
   is
      type Unsigned_32_Acc is access Unsigned_32;
      function To_Unsigned_32_Acc is new Ada.Unchecked_Conversion
        (Source => Address, Target => Unsigned_32_Acc);

      W : Unsigned_32;
      Lo : Natural;

      --  Add CHAR to the line.
      procedure Add_Char (C : Character);
      pragma Inline (Add_Char);

      procedure Add_Char (C : Character) is
      begin
         Line (Lo) := C;
         Lo := Lo + 1;
      end Add_Char;

      --  Add STR to the line.
      procedure Add_String (Str : String) is
      begin
         Line (Lo .. Lo + Str'Length - 1) := Str;
         Lo := Lo + Str'Length;
      end Add_String;

      --  Add BYTE to the line.
--       procedure Add_Byte (V : Byte) is
--          type My_Str is array (Natural range 0 .. 15) of Character;
--          Hex_Digit : constant My_Str := "0123456789abcdef";
--       begin
--          Add_Char (Hex_Digit (Natural (Shift_Right (V, 4) and 16#0f#)));
--          Add_Char (Hex_Digit (Natural (Shift_Right (V, 0) and 16#0f#)));
--       end Add_Byte;

      procedure Disp_Const (Mask : Unsigned_32)
      is
         L : Natural;
         V : Unsigned_32;
      begin
         L := Lo;
         Proc_Cb.all (Addr, Line (Lo .. Line'Last), Lo);
         V := W and Mask;

         -- Extend sign.
         if (W and ((Mask + 1) / 2)) /= 0 then
            V := V or not Mask;
         end if;
         if L /= Lo then
            if V = 0 then
               return;
            end if;
            Add_String (" + ");
         end if;
         Add_String ("0x");
         Add_String (Hex_Image (V));
      end Disp_Const;

      procedure Add_Cond (Str : String)
      is
      begin
         Add_String (Str);
         Add_String (Bicc_Map (Shift_Right (W, 25) and 2#1111#).all);
         if (W and 16#2000_0000#) /= 0 then
            Add_String (",a");
         end if;
         Add_Char (' ');
         Disp_Const (16#3f_Ffff#);
      end Add_Cond;


      procedure Add_Ireg (R : Reg_Type)
      is
      begin
         Add_Char ('%');
         if R <= 7 then
            Add_Char ('g');
         elsif R <= 15 then
            if R = 14 then
               Add_String ("sp");
               return;
            else
               Add_Char ('o');
            end if;
         elsif R <= 23 then
            Add_Char ('l');
         else
            if R = 30 then
               Add_String ("fp");
               return;
            else
               Add_Char ('i');
            end if;
         end if;
         Add_Char (Hex_Digit (R and 7));
      end Add_Ireg;

      procedure Disp_Unknown is
      begin
         Add_String ("unknown ");
         Add_String (Hex_Image (W));
      end Disp_Unknown;

      procedure Disp_Format3 (Map : Insn_Desc_Array)
      is
         Op2 : Unsigned_32 range 0 .. 63;
      begin
         Op2 := Shift_Right (W, 19) and 2#111_111#;

         case Map (Op2).Format is
            when Format_Regimm =>
               Add_String (Map (Op2).Name.all);
               Add_Char (' ');
               Add_Ireg (Shift_Right (W, 25) and 31);
               Add_Char (',');
               Add_Ireg (Shift_Right (W, 14) and 31);
               Add_Char (',');
               if (W and 16#2000#) /= 0 then
                  Disp_Const (16#1fff#);
               else
                  Add_Ireg (W and 31);
               end if;
            when others =>
               Add_String ("unknown3, op2=");
               Add_String (Hex_Image (Op2));
         end case;
      end Disp_Format3;


   begin
      W := To_Unsigned_32_Acc (Addr).all;
      Insn_Len := 4;
      Lo := Line'First;

      case Shift_Right (W, 30) is
         when 2#00# =>
            --  BIcc, SETHI
            case Shift_Right (W, 22) and 2#111# is
               when 2#000# =>
                  Add_String ("unimp ");
                  Disp_Const (16#3f_Ffff#);
               when 2#010# =>
                  Add_Cond ("b");
               when 2#100# =>
                  Add_String ("sethi ");
                  Add_Ireg (Shift_Right (W, 25));
                  Add_String (", ");
                  Disp_Const (16#3f_Ffff#);
               when others =>
                  Disp_Unknown;
            end case;
         when 2#01# =>
            --  Call
            Add_String ("call ");
            Disp_Const (16#3fff_Ffff#);
         when 2#10# =>
            Disp_Format3 (Insn_Desc_10);
         when 2#11# =>
            Disp_Format3 (Insn_Desc_11);
         when others =>
            --  Misc.
            Disp_Unknown;
      end case;

      Line_Len := Lo - Line'First;
   end Disassemble_Insn;

end Disa_Sparc;
