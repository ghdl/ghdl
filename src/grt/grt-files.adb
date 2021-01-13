--  GHDL Run Time (GRT) -  VHDL files subprograms.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Grt.Errors; use Grt.Errors;
with Grt.Errors_Exec; use Grt.Errors_Exec;
with Grt.Stdio; use Grt.Stdio;
with Grt.C; use Grt.C;
with Grt.Table;
with Grt.Options;
with System; use System;
pragma Elaborate_All (Grt.Table);

package body Grt.Files is
   subtype C_Files is Grt.Stdio.FILEs;

   --  The end of lines
   C_LF : constant int := 10;   --  \n
   C_CR : constant int := 13;   --  \r

   Auto_Flush : constant Boolean := False;

   type File_Entry_Type is record
      --  The corresponding C stream.
      Stream : C_Files;

      Signature : Ghdl_C_String;

      --  Open kind: r, a or w.
      Kind : Character;

      Is_Text : Boolean;

      --  True if the file entry is used.
      Is_Alive : Boolean;
   end record;

   package Files_Table is new Grt.Table
     (Table_Component_Type => File_Entry_Type,
      Table_Index_Type => Ghdl_File_Index,
      Table_Low_Bound => 1,
      Table_Initial => 2);

   function Get_File (Index : Ghdl_File_Index) return C_Files is
   begin
      if Index not in Files_Table.First .. Files_Table.Last then
         Internal_Error ("get_file: bad file index");
      end if;
      return Files_Table.Table (Index).Stream;
   end Get_File;

   function Is_Open (Index : Ghdl_File_Index) return Boolean is
   begin
      return Files_Table.Table (Index).Stream /= NULL_Stream;
   end Is_Open;

   function Get_Kind (Index : Ghdl_File_Index) return Character is
   begin
      return Files_Table.Table (Index).Kind;
   end Get_Kind;

   procedure Check_File_Mode (Index : Ghdl_File_Index; Is_Text : Boolean) is
   begin
      if Files_Table.Table (Index).Is_Text /= Is_Text then
         Internal_Error ("check_file_mode: bad file mode");
      end if;
   end Check_File_Mode;

   procedure Check_Read (Index : Ghdl_File_Index; Is_Text : Boolean) is
   begin
      Check_File_Mode (Index, Is_Text);

      --  LRM08 5.5.2 File operations
      --  It is an error if the access mode of the file object is write-only
      --  or if the file object is not open.
      if not Is_Open (Index) then
         Error_Call_Stack ("read called on a non-open file", 2);
      end if;
      if Get_Kind (Index) /= 'r' then
         Error_Call_Stack ("read called on a write-only file", 2);
      end if;
   end Check_Read;

   procedure Check_Write (Index : Ghdl_File_Index; Is_Text : Boolean) is
   begin
      Check_File_Mode (Index, Is_Text);

      --  LRM08 5.5.2 File operations
      --  It is an error if the access mode of the file object is read-only
      --  or if the file object is not open.
      if not Is_Open (Index) then
         Error_Call_Stack ("write called on a non-open file", 2);
      end if;
      if Get_Kind (Index) = 'r' then
         Error_Call_Stack ("write called on a read-only file", 2);
      end if;
   end Check_Write;

   function Create_File
     (Is_Text : Boolean; Kind : Character; Sig : Ghdl_C_String)
     return Ghdl_File_Index is
   begin
      Files_Table.Append ((Stream => NULL_Stream,
                           Signature => Sig,
                           Kind => Kind,
                           Is_Text => Is_Text,
                           Is_Alive => True));
      return Files_Table.Last;
   end Create_File;

   procedure Destroy_File (Is_Text : Boolean; Index : Ghdl_File_Index) is
   begin
      if Get_File (Index) /= NULL_Stream then
         Internal_Error ("destroy_file");
      end if;
      Check_File_Mode (Index, Is_Text);
      Files_Table.Table (Index).Is_Alive := False;
      if Index = Files_Table.Last then
         while Files_Table.Last >= Files_Table.First
           and then Files_Table.Table (Files_Table.Last).Is_Alive = False
         loop
            Files_Table.Decrement_Last;
         end loop;
      end if;
   end Destroy_File;

   procedure File_Error (File : Ghdl_File_Index)
   is
      pragma Unreferenced (File);
   begin
      Internal_Error ("file: IO error");
   end File_Error;

   function Ghdl_Text_File_Elaborate return Ghdl_File_Index is
   begin
      return Create_File (True, ' ', null);
   end Ghdl_Text_File_Elaborate;

   function Ghdl_File_Elaborate (Sig : Ghdl_C_String) return Ghdl_File_Index
   is
   begin
      return Create_File (False, ' ', Sig);
   end Ghdl_File_Elaborate;

   procedure Ghdl_Text_File_Finalize (File : Ghdl_File_Index) is
   begin
      Destroy_File (True, File);
   end Ghdl_Text_File_Finalize;

   procedure Ghdl_File_Finalize (File : Ghdl_File_Index) is
   begin
      Destroy_File (False, File);
   end Ghdl_File_Finalize;

   function Ghdl_File_Endfile (File : Ghdl_File_Index) return Boolean
   is
      Stream : C_Files;
      C : int;
   begin
      Stream := Get_File (File);

      --  LRM93 3.4.1 File Operations
      --  LRM08 5.5.2 File Operations
      --  It is an error if ENDFILE is called on a file object that is not
      --  open.
      if Stream = NULL_Stream then
         Error_Call_Stack ("endfile with a non-opened file", 1);
      end if;

      --  LRM93 3.4.1 File Operations
      --  LRM08 5.5.2 File Operations
      --  Function ENDFILE always returns TRUE for an open file object whose
      --  access mode is write-only.
      if Get_Kind (File) /= 'r' then
         return True;
      end if;

      if feof (Stream) /= 0 then
         return True;
      end if;
      C := fgetc (Stream);
      if C < 0 then
         return True;
      end if;
      if ungetc (C, Stream) /= C then
         Error ("internal error: ungetc");
      end if;
      return False;
   end Ghdl_File_Endfile;

   Sig_Header : constant String := "#GHDL-BINARY-FILE-0.0" & Nl;

   function File_Open (File : Ghdl_File_Index;
                       Mode : Ghdl_I32;
                       Str : Std_String_Ptr)
     return Ghdl_I32
   is
      Name : String (1 .. Integer (Str.Bounds.Dim_1.Length) + 1);
      Str_Mode : String (1 .. 3);
      F : C_Files;
      Sig : Ghdl_C_String;
      Sig_Len : Natural;
      Kind : Character;
   begin
      F := Get_File (File);

      if F /= NULL_Stream then
         --  File was already open.
         return Status_Error;
      end if;

      --  Copy file name and convert it to a C string (NUL terminated).
      for I in 1 .. Str.Bounds.Dim_1.Length loop
         Name (Natural (I)) := Str.Base (I - 1);
      end loop;
      Name (Name'Last) := NUL;

      case Mode is
         when Read_Mode =>
            Kind := 'r';
         when Write_Mode =>
            Kind := 'w';
         when Append_Mode =>
            Kind := 'a';
         when others =>
            --  Bad mode, cannot happen.
            Internal_Error ("file_open: bad open mode");
      end case;

      if Name = "STD_INPUT" & NUL then
         if Mode /= Read_Mode then
            return Mode_Error;
         end if;
         F := stdin;
      elsif Name = "STD_OUTPUT" & NUL then
         if Mode /= Write_Mode then
            return Mode_Error;
         end if;
         F := stdout;
      else
         Str_Mode (1) := Kind;
         if Files_Table.Table (File).Is_Text then
            Str_Mode (2) := NUL;
         else
            Str_Mode (2) := 'b';
            Str_Mode (3) := NUL;
         end if;
         F := fopen (Name'Address, Str_Mode'Address);
         if F = NULL_Stream then
            return Name_Error;
         end if;
         if Grt.Options.Unbuffered_Writes and Mode /= Read_Mode then
            setbuf (F, NULL_voids);
         end if;
      end if;

      Sig := Files_Table.Table (File).Signature;
      if Sig /= null then
         Sig_Len := strlen (Sig);
         case Mode is
            when Write_Mode =>
               if fwrite (Sig_Header'Address, 1, Sig_Header'Length, F)
                 /= Sig_Header'Length
               then
                  File_Error (File);
               end if;
               if fwrite (Sig (1)'Address, 1, size_t (Sig_Len), F)
                 /= size_t (Sig_Len)
               then
                  File_Error (File);
               end if;
            when Read_Mode =>
               declare
                  Hdr : String (1 .. Sig_Header'Length);
                  Sig_Buf : String (1 .. Sig_Len);
               begin
                  if fread (Hdr'Address, 1, Hdr'Length, F) /= Hdr'Length then
                     File_Error (File);
                  end if;
                  if Hdr /= Sig_Header then
                     File_Error (File);
                  end if;
                  if fread (Sig_Buf'Address, 1, Sig_Buf'Length, F)
                    /= Sig_Buf'Length
                  then
                     File_Error (File);
                  end if;
                  if Sig_Buf /= Sig (1 .. Sig_Len) then
                     File_Error (File);
                  end if;
               end;
            when Append_Mode =>
               null;
            when others =>
               null;
         end case;
      end if;

      Files_Table.Table (File).Stream := F;
      Files_Table.Table (File).Kind := Kind;

      return Open_Ok;
   end File_Open;

   procedure Error_Open (Str : Std_String_Ptr)
   is
      Bt : Backtrace_Addrs;
   begin
      Save_Backtrace (Bt, 2);
      Error_S ("cannot open file """);
      Diag_C_Std (Str.Base (0 .. Str.Bounds.Dim_1.Length - 1));
      Diag_C ('"');
      Error_E_Call_Stack (Bt);
   end Error_Open;

   procedure Ghdl_Text_File_Open
     (File : Ghdl_File_Index; Mode : Ghdl_I32; Str : Std_String_Ptr)
   is
      Res : Ghdl_I32;
   begin
      Check_File_Mode (File, True);

      Res := File_Open (File, Mode, Str);

      if Res /= Open_Ok then
         Error_Open (Str);
      end if;
   end Ghdl_Text_File_Open;

   procedure Ghdl_File_Open
     (File : Ghdl_File_Index; Mode : Ghdl_I32; Str : Std_String_Ptr)
   is
      Res : Ghdl_I32;
   begin
      Check_File_Mode (File, False);

      Res := File_Open (File, Mode, Str);

      if Res /= Open_Ok then
         Error_Open (Str);
      end if;
   end Ghdl_File_Open;

   function Ghdl_Text_File_Open_Status
     (File : Ghdl_File_Index; Mode : Ghdl_I32; Str : Std_String_Ptr)
     return Ghdl_I32
   is
   begin
      Check_File_Mode (File, True);
      return File_Open (File, Mode, Str);
   end Ghdl_Text_File_Open_Status;

   function Ghdl_File_Open_Status
     (File : Ghdl_File_Index; Mode : Ghdl_I32; Str : Std_String_Ptr)
     return Ghdl_I32
   is
   begin
      Check_File_Mode (File, False);
      return File_Open (File, Mode, Str);
   end Ghdl_File_Open_Status;

   procedure Ghdl_Text_Write (File : Ghdl_File_Index; Str : Std_String_Ptr)
   is
      Res : C_Files;
      Len : size_t;
      R : size_t;
   begin
      Res := Get_File (File);
      Check_Write (File, True);

      Len := size_t (Str.Bounds.Dim_1.Length);
      if Len = 0 then
         return;
      end if;

      R := fwrite (Str.Base (0)'Address, Len, 1, Res);
      if R /= 1 then
         Error ("text_write failed");
      end if;

      if Auto_Flush then
         fflush (Res);
      end if;
   end Ghdl_Text_Write;

   procedure Ghdl_Write_Scalar (File : Ghdl_File_Index;
                                Ptr : Ghdl_Ptr;
                                Length : Ghdl_Index_Type)
   is
      Res : C_Files;
      R : size_t;
   begin
      Res := Get_File (File);
      Check_Write (File, False);

      R := fwrite (System.Address (Ptr), size_t (Length), 1, Res);
      if R /= 1 then
         Error ("write_scalar failed");
      end if;
      if Auto_Flush then
         fflush (Res);
      end if;
   end Ghdl_Write_Scalar;

   procedure Ghdl_Read_Scalar (File : Ghdl_File_Index;
                               Ptr : Ghdl_Ptr;
                               Length : Ghdl_Index_Type)
   is
      Res : C_Files;
      R : size_t;
   begin
      Res := Get_File (File);
      Check_Read (File, False);

      R := fread (System.Address (Ptr), size_t (Length), 1, Res);
      if R /= 1 then
         Error ("read_scalar failed");
      end if;
   end Ghdl_Read_Scalar;

   function Ghdl_Text_Read_Length (File : Ghdl_File_Index;
                                   Str : Std_String_Ptr)
     return Std_Integer
   is
      Stream : C_Files;
      C : int;
      Len : Ghdl_Index_Type;
   begin
      Stream := Get_File (File);
      Check_Read (File, True);

      Len := Str.Bounds.Dim_1.Length;
      --  Read until EOL (or EOF).
      --  Store as much as possible.
      for I in Ghdl_Index_Type loop
         C := fgetc (Stream);
         if C < 0 then
            Error_Call_Stack ("read: end of file reached", 1);
            return Std_Integer (I);
         end if;
         if I < Len then
            Str.Base (I) := Character'Val (C);
         end if;
         --  End of line is '\n' or LF or character # 10.
         if C = 10 then
            return Std_Integer (I + 1);
         end if;
      end loop;
      return 0;
   end Ghdl_Text_Read_Length;

   procedure Ghdl_Untruncated_Text_Read
     (File : Ghdl_File_Index; Str : Std_String_Ptr; Len : Std_Integer_Acc)
   is
      Stream : C_Files;
      Max_Len : int;
      C : int;
      L : Ghdl_Index_Type;
   begin
      Stream := Get_File (File);
      Check_Read (File, True);

      Max_Len := int (Str.Bounds.Dim_1.Length);

      --  Read at most LEN characters, stop at EOL.
      L := 0;
      for I in 1 .. Max_Len loop
         C := fgetc (Stream);
         exit when C < 0;
         --  Be nice with DOS files: handle CR/CR+LF/LF.
         --  Note: LF+CR is not handled, so that on unix we don't need
         --  to read the next line.
         --  Always return LF as end of line.
         if C = C_CR then
            C := fgetc (Stream);
            if C > 0 and C /= C_LF then
               C := ungetc (C, Stream);
               pragma Assert (C >= 0);
            end if;
            C := C_LF;
         end if;
         Str.Base (L) := Character'Val (C);
         L := L + 1;
         exit when C = C_LF;
      end loop;

      Len.all := Std_Integer (L);
   end Ghdl_Untruncated_Text_Read;

   procedure File_Close (File : Ghdl_File_Index; Is_Text : Boolean)
   is
      Stream : C_Files;
   begin
      Stream := Get_File (File);
      Check_File_Mode (File, Is_Text);

      --  LRM 3.4.1  File Operations
      --  If F is not associated with an external file, then FILE_CLOSE has
      --  no effect.
      if Stream = NULL_Stream then
         return;
      end if;

      if fclose (Stream) /= 0 then
         Internal_Error ("file_close: fclose error");
      end if;
      Files_Table.Table (File).Stream := NULL_Stream;
   end File_Close;

   procedure Ghdl_Text_File_Close (File : Ghdl_File_Index) is
   begin
      File_Close (File, True);
   end Ghdl_Text_File_Close;

   procedure Ghdl_File_Close (File : Ghdl_File_Index) is
   begin
      File_Close (File, False);
   end Ghdl_File_Close;

   procedure Ghdl_File_Flush (File : Ghdl_File_Index)
   is
      Stream : C_Files;
   begin
      Stream := Get_File (File);

      --  LRM08 5.5.2 File Operations
      --  For the WRITE and FLUSH procedures, it is an error if the access
      --  mode of the file object is read-only or if the file is not open.
      if Stream = NULL_Stream then
         Error_Call_Stack ("flush called on a non-open file", 1);
      end if;
      if Get_Kind (File) = 'r' then
         Error_Call_Stack ("flush called on a read-only file", 1);
      end if;

      fflush (Stream);
   end Ghdl_File_Flush;
end Grt.Files;
