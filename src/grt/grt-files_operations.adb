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
with Grt.Stdio; use Grt.Stdio;
with Grt.C; use Grt.C;
with Grt.Table;
with System; use System;
pragma Elaborate_All (Grt.Table);

package body Grt.Files_Operations is
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

   --  Get the C stream for INDEX.
   procedure Get_File
     (Index : Ghdl_File_Index; Res : out C_Files; Status : out Op_Status) is
   begin
      if Index not in Files_Table.First .. Files_Table.Last then
         Status := Op_Bad_Index;
      else
         Status := Op_Ok;
         Res := Files_Table.Table (Index).Stream;
      end if;
   end Get_File;

   --  Assume INDEX is correct.
   function Is_Open (Index : Ghdl_File_Index) return Boolean is
   begin
      return Files_Table.Table (Index).Stream /= NULL_Stream;
   end Is_Open;

   --  Assume INDEX is correct.
   function Get_Kind (Index : Ghdl_File_Index) return Character is
   begin
      return Files_Table.Table (Index).Kind;
   end Get_Kind;

   procedure Check_File_Mode
     (Index : Ghdl_File_Index; Is_Text : Boolean; Status : out Op_Status) is
   begin
      if Files_Table.Table (Index).Is_Text /= Is_Text then
         Status := Op_Bad_Mode;
      else
         Status := Op_Ok;
      end if;
   end Check_File_Mode;

   procedure Check_Read
     (Index : Ghdl_File_Index; Is_Text : Boolean; Status : out Op_Status) is
   begin
      Check_File_Mode (Index, Is_Text, Status);
      if Status /= Op_Ok then
         return;
      end if;

      --  LRM08 5.5.2 File operations
      --  It is an error if the access mode of the file object is write-only
      --  or if the file object is not open.
      if not Is_Open (Index) then
         Status := Op_Not_Open;
         return;
      end if;
      if Get_Kind (Index) /= 'r' then
         Status := Op_Read_Write_File;
         return;
      end if;

      Status := Op_Ok;
   end Check_Read;

   procedure Check_Write
     (Index : Ghdl_File_Index; Is_Text : Boolean; Status : out Op_Status) is
   begin
      Check_File_Mode (Index, Is_Text, Status);
      if Status /= Op_Ok then
         return;
      end if;

      --  LRM08 5.5.2 File operations
      --  It is an error if the access mode of the file object is read-only
      --  or if the file object is not open.
      if not Is_Open (Index) then
         Status := Op_Not_Open;
         return;
      end if;
      if Get_Kind (Index) = 'r' then
         Status := Op_Write_Read_File;
         return;
      end if;

      Status := Op_Ok;
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

   procedure Destroy_File
     (Is_Text : Boolean; Index : Ghdl_File_Index; Status : out Op_Status)
   is
      Cstream : C_Files;
   begin
      Get_File (Index, Cstream, Status);
      if Status /= Op_Ok then
         return;
      end if;
      if Cstream /= NULL_Stream then
         Status := Op_Not_Closed;
         return;
      end if;
      Check_File_Mode (Index, Is_Text, Status);
      if Status /= Op_Ok then
         return;
      end if;

      --  Cleanup.
      Files_Table.Table (Index).Is_Alive := False;
      if Index = Files_Table.Last then
         while Files_Table.Last >= Files_Table.First
           and then Files_Table.Table (Files_Table.Last).Is_Alive = False
         loop
            Files_Table.Decrement_Last;
         end loop;
      end if;
   end Destroy_File;

   function Ghdl_Text_File_Elaborate return Ghdl_File_Index is
   begin
      return Create_File (True, ' ', null);
   end Ghdl_Text_File_Elaborate;

   function Ghdl_File_Elaborate (Sig : Ghdl_C_String) return Ghdl_File_Index
   is
   begin
      return Create_File (False, ' ', Sig);
   end Ghdl_File_Elaborate;

   procedure Ghdl_Text_File_Finalize
     (File : Ghdl_File_Index; Status : out Op_Status) is
   begin
      Destroy_File (True, File, Status);
   end Ghdl_Text_File_Finalize;

   procedure Ghdl_File_Finalize
     (File : Ghdl_File_Index; Status : out Op_Status) is
   begin
      Destroy_File (False, File, Status);
   end Ghdl_File_Finalize;

   procedure Ghdl_File_Endfile
     (File : Ghdl_File_Index; Status : out Op_Status)
   is
      Stream : C_Files;
      C : int;
   begin
      Get_File (File, Stream, Status);
      if Status /= Op_Ok then
         return;
      end if;

      --  LRM93 3.4.1 File Operations
      --  LRM08 5.5.2 File Operations
      --  It is an error if ENDFILE is called on a file object that is not
      --  open.
      if Stream = NULL_Stream then
         Status := Op_Not_Open;
         return;
      end if;

      --  Default: returns True.
      Status := Op_End_Of_File;

      --  LRM93 3.4.1 File Operations
      --  LRM08 5.5.2 File Operations
      --  Function ENDFILE always returns TRUE for an open file object whose
      --  access mode is write-only.
      if Get_Kind (File) /= 'r' then
         return;
      end if;

      if feof (Stream) /= 0 then
         return;
      end if;
      C := fgetc (Stream);
      if C < 0 then
         return;
      end if;
      if ungetc (C, Stream) /= C then
         Status := Op_Ungetc_Error;
         return;
      end if;

      Status := Op_Ok;
      return;
   end Ghdl_File_Endfile;

   function Simple_Open (Name : Ghdl_C_String; Mode : Ghdl_C_String)
                        return C_Files is
   begin
      return fopen (To_Address (Name), To_Address (Mode));
   end Simple_Open;

   Sig_Header : constant String := "#GHDL-BINARY-FILE-0.0" & Nl;

   Std_Output_Name : constant String := "STD_OUTPUT" & NUL;
   Std_Input_Name : constant String := "STD_INPUT" & NUL;

   procedure File_Open (File : Ghdl_File_Index;
                        Mode : Ghdl_I32;
                        Name : Ghdl_C_String;
                        Status : out Op_Status)
   is
      Str_Mode : String (1 .. 3);
      F : C_Files;
      Sig : Ghdl_C_String;
      Sig_Len : Natural;
      Kind : Character;
   begin
      Get_File (File, F, Status);
      if Status /= Op_Ok then
         return;
      end if;

      if F /= NULL_Stream then
         --  File was already open.
         Status := Op_Not_Closed;
         return;
      end if;

      case Mode is
         when Read_Mode =>
            Kind := 'r';
         when Write_Mode =>
            Kind := 'w';
         when Append_Mode =>
            Kind := 'a';
         when others =>
            --  Bad mode, cannot happen.
            Status := Op_Bad_Mode;
            return;
      end case;

      if strcmp (Name, To_Ghdl_C_String (Std_Input_Name'Address)) = 0 then
         if Mode /= Read_Mode then
            Status := Op_Mode_Error;
            return;
         end if;
         F := stdin;
      elsif strcmp (Name, To_Ghdl_C_String (Std_Output_Name'Address)) = 0 then
         if Mode /= Write_Mode then
            Status := Op_Mode_Error;
            return;
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
         F := Open_Handler (Name, To_Ghdl_C_String (Str_Mode'Address));
         if F = NULL_Stream then
            Status := Op_Name_Error;
            return;
         end if;
         -- if Grt.Options.Unbuffered_Writes and Mode /= Read_Mode then
         --    setbuf (F, NULL_voids);
         -- end if;
      end if;

      Sig := Files_Table.Table (File).Signature;
      if Sig /= null then
         Sig_Len := strlen (Sig);
         case Mode is
            when Write_Mode =>
               if fwrite (Sig_Header'Address, 1, Sig_Header'Length, F)
                 /= Sig_Header'Length
               then
                  Status := Op_Write_Error;
                  return;
               end if;
               if fwrite (Sig (1)'Address, 1, size_t (Sig_Len), F)
                 /= size_t (Sig_Len)
               then
                  Status := Op_Write_Error;
                  return;
               end if;
            when Read_Mode =>
               declare
                  Hdr : String (1 .. Sig_Header'Length);
                  Sig_Buf : String (1 .. Sig_Len);
               begin
                  if fread (Hdr'Address, 1, Hdr'Length, F) /= Hdr'Length then
                     Status := Op_Read_Error;
                     return;
                  end if;
                  if Hdr /= Sig_Header then
                     Status := Op_Signature_Error;
                     return;
                  end if;
                  if fread (Sig_Buf'Address, 1, Sig_Buf'Length, F)
                    /= Sig_Buf'Length
                  then
                     Status := Op_Read_Error;
                     return;
                  end if;
                  if Sig_Buf /= Sig (1 .. Sig_Len) then
                     Status := Op_Signature_Error;
                     return;
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

      Status := Op_Ok;
   end File_Open;

   procedure Ghdl_Text_File_Open (File : Ghdl_File_Index;
                                  Mode : Ghdl_I32;
                                  Name : Ghdl_C_String;
                                  Status : out Op_Status) is
   begin
      Check_File_Mode (File, True, Status);
      if Status /= Op_Ok then
         return;
      end if;

      File_Open (File, Mode, Name, Status);
   end Ghdl_Text_File_Open;

   procedure Ghdl_File_Open (File : Ghdl_File_Index;
                             Mode : Ghdl_I32;
                             Name : Ghdl_C_String;
                             Status : out Op_Status) is
   begin
      Check_File_Mode (File, False, Status);
      if Status /= Op_Ok then
         return;
      end if;

      File_Open (File, Mode, Name, Status);
   end Ghdl_File_Open;

   procedure Ghdl_Text_Write (File : Ghdl_File_Index; Str : Std_String_Ptr;
                                                      Status : out Op_Status)
   is
      Res : C_Files;
      Len : size_t;
      R : size_t;
   begin
      Get_File (File, Res, Status);
      if Status /= Op_Ok then
         return;
      end if;
      Check_Write (File, True, Status);
      if Status /= Op_Ok then
         return;
      end if;

      Len := size_t (Str.Bounds.Dim_1.Length);
      if Len = 0 then
         Status := Op_Ok;
         return;
      end if;

      R := fwrite (Str.Base (0)'Address, Len, 1, Res);
      if R /= 1 then
         Status := Op_Write_Error;
         return;
      end if;

      if Auto_Flush then
         fflush (Res);
      end if;

      Status := Op_Ok;
   end Ghdl_Text_Write;

   procedure Ghdl_Write_Scalar (File : Ghdl_File_Index;
                                Ptr : Ghdl_Ptr;
                                Length : Ghdl_Index_Type;
                                Status : out Op_Status)
   is
      Res : C_Files;
      R : size_t;
   begin
      Get_File (File, Res, Status);
      if Status /= Op_Ok then
         return;
      end if;
      Check_Write (File, False, Status);
      if Status /= Op_Ok then
         return;
      end if;

      R := fwrite (System.Address (Ptr), size_t (Length), 1, Res);
      if R /= 1 then
         Status := Op_Write_Error;
         return;
      end if;
      if Auto_Flush then
         fflush (Res);
      end if;

      Status := Op_Ok;
   end Ghdl_Write_Scalar;

   procedure Ghdl_Read_Scalar (File : Ghdl_File_Index;
                               Ptr : Ghdl_Ptr;
                               Length : Ghdl_Index_Type;
                               Status : out Op_Status)
   is
      Res : C_Files;
      R : size_t;
   begin
      Get_File (File, Res, Status);
      if Status /= Op_Ok then
         return;
      end if;
      Check_Read (File, False, Status);
      if Status /= Op_Ok then
         return;
      end if;

      R := fread (System.Address (Ptr), size_t (Length), 1, Res);
      if R /= 1 then
         Status := Op_Read_Error;
         return;
      end if;

      Status := Op_Ok;
   end Ghdl_Read_Scalar;

   procedure Ghdl_Text_Read_Length (File : Ghdl_File_Index;
                                    Str : Std_String_Ptr;
                                    Status : out Op_Status;
                                    Length : out Std_Integer)
   is
      Stream : C_Files;
      C : int;
      Len : Ghdl_Index_Type;
   begin
      Length := 0;
      Get_File (File, Stream, Status);
      if Status /= Op_Ok then
         return;
      end if;
      Check_Read (File, True, Status);
      if Status /= Op_Ok then
         return;
      end if;

      Len := Str.Bounds.Dim_1.Length;
      --  Read until EOL (or EOF).
      --  Store as much as possible.
      for I in Ghdl_Index_Type loop
         C := fgetc (Stream);
         if C < 0 then
            Length := Std_Integer (I);
            Status := Op_End_Of_File;
            return;
         end if;
         if I < Len then
            Str.Base (I) := Character'Val (C);
         end if;
         --  End of line is '\n' or LF or character # 10.
         if C = C_LF then
            Length := Std_Integer (I + 1);
            Status := Op_Ok;
            return;
         end if;
      end loop;
      Length := 0;
      Status := Op_Ok;
   end Ghdl_Text_Read_Length;

   procedure Ghdl_Untruncated_Text_Read (File : Ghdl_File_Index;
                                         Buf : Ghdl_C_String;
                                         Len : in out Std_Integer;
                                         Status : out Op_Status)
   is
      Stream : C_Files;
      L : Natural;
      C : int;
   begin
      Get_File (File, Stream, Status);
      if Status /= Op_Ok then
         return;
      end if;
      Check_Read (File, True, Status);
      if Status /= Op_Ok then
         return;
      end if;

      --  Default status.
      Status := Op_Ok;

      --  Read at most LEN characters, stop at EOL.
      L := 0;
      for I in 1 .. Len loop
         C := fgetc (Stream);
         if C < 0 then
            Status := Op_End_Of_File;
            exit;
         end if;
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
         L := L + 1;
         Buf (L) := Character'Val (C);
         exit when C = C_LF;
      end loop;

      Len := Std_Integer (L);
   end Ghdl_Untruncated_Text_Read;

   procedure File_Close
     (File : Ghdl_File_Index; Is_Text : Boolean; Status : out Op_Status)
   is
      Stream : C_Files;
   begin
      Get_File (File, Stream, Status);
      if Status /= Op_Ok then
         return;
      end if;
      Check_File_Mode (File, Is_Text, Status);
      if Status /= Op_Ok then
         return;
      end if;

      --  LRM 3.4.1  File Operations
      --  If F is not associated with an external file, then FILE_CLOSE has
      --  no effect.
      if Stream = NULL_Stream then
         Status := Op_Ok;
         return;
      end if;

      if fclose (Stream) /= 0 then
         Status := Op_Close_Error;
         return;
      end if;
      Files_Table.Table (File).Stream := NULL_Stream;
      Status := Op_Ok;
   end File_Close;

   procedure Ghdl_Text_File_Close
     (File : Ghdl_File_Index; Status : out Op_Status) is
   begin
      File_Close (File, True, Status);
   end Ghdl_Text_File_Close;

   procedure Ghdl_File_Close
     (File : Ghdl_File_Index; Status : out Op_Status) is
   begin
      File_Close (File, False, Status);
   end Ghdl_File_Close;

   procedure Ghdl_File_Flush (File : Ghdl_File_Index; Status : out Op_Status)
   is
      Stream : C_Files;
   begin
      Get_File (File, Stream, Status);
      if Status /= Op_Ok then
         return;
      end if;

      --  LRM08 5.5.2 File Operations
      --  For the WRITE and FLUSH procedures, it is an error if the access
      --  mode of the file object is read-only or if the file is not open.
      if Stream = NULL_Stream then
         Status := Op_Not_Open;
         return;
      end if;
      if Get_Kind (File) = 'r' then
         Status := Op_Write_Read_File;
         return;
      end if;

      fflush (Stream);
      Status := Op_Ok;
   end Ghdl_File_Flush;
end Grt.Files_Operations;
