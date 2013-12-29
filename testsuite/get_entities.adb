----------------------------------------------------------------------------
--      get_entities (get_entities.adb)
--
--               Copyright (C) 2013, Brian Drummond
--
--      This file is part of the ghdl-updates project.
--
--      get_entities is free software: you can redistribute it and/or modify
--      it under the terms of the GNU General Public License as published by
--      the Free Software Foundation, either version 2 of the License, or
--      (at your option) any later version.
--
--      get_entities is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the GNU General Public License
--      along with get_entities.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------

with Ada.Text_Io; use Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Command_Line;

procedure get_entities is

   function Valid_Test(Name : in String) return boolean is
      use Ada.Directories;
      use Ada.Characters.Handling;
   begin
      return Extension(To_Lower(Name)) = "vhd"
        or Extension(To_Lower(Name)) = "vhdl";
   end Valid_Test;

   procedure Get_Top_Entities(Test_Name : in String) is
      use Ada.Text_Io;
      use Ada.Strings.Fixed;
      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded;

      File : File_Type;

      function Get_End(Line : in String) return Natural is
         Comment : natural := Index(Line,"--");
      begin
         if Comment = 0 then
            return Line'last;
         else
            return Comment - 1;
         end if;
      end Get_End;

      type State_Type is (Idle, Have_Entity, Have_Name, In_Entity);
      State : State_Type;

      Top_Level_Entity : Boolean;
      Name : Unbounded_String;

      Last_Entity : Unbounded_String;
   begin
   -- Return the name of all top-level entities in the file.
   -- Report on stderr, a malformed one
   -- "malformed" means not conforming to the expectations of this simple parser.
   -- A top level entity has the form
   -- Entity <name> is
   -- <no port clause>
   -- end {entity} <name>

      Open(File, In_File, Test_Name);
      State := Idle;
      loop
         declare
            -- strip name of blanks etc...
            CharSet       : constant Ada.Strings.Maps.Character_Ranges := (('A','Z'), ('a','z'), ('0','9'), ('_','_'));

            function Token(Source, Name : String; From : positive := 1) return natural is
               use Ada.Strings.Maps;
               Pos : natural := Index(Source, Name, From => From);
            begin
               -- Look in Source for Name, either surrounded by whitespace or at the start or end of a line
               if Pos = 0 or Pos = 1 or Pos + Name'Length > Source'Length then
                  return Pos;
               elsif not is_in (Source(Pos - 1), To_Set(CharSet)) and
                     not is_in (Source(Pos + Name'Length), To_Set(CharSet)) then
                  return Pos;
               else
                  return 0;
               end if;
            end Token;

            function  Strip_Quoted(Raw : String) return String is
               temp : String(Raw'range);
               t    : natural := Raw'first;
               copy : Boolean := true;
            begin
               -- Eliminate quoted text
               for i in Raw'range loop
                  if copy then
                     if Raw(i) = '"' then
                        copy := not copy;
                     else
                        temp(t) := Raw(i);
                        t := t + 1;
                     end if;
                  else
                     if Raw(i) = '"' then
                        copy := not copy;
                     end if;
                  end if;
               end loop;
               if t > Raw'last then t := Raw'last; end if;
               return temp(Raw'first .. t);
            end Strip_Quoted;

	    Line    : String  := Get_Line (File); -- line based to strip off comments
            EndLine : natural := Get_End (Line);
            Raw     : String  := To_Lower(Line (1 .. EndLine));
            Code    : String  := Strip_Quoted(Raw);
            -- positions of specific strings in a line.
            Ent     : Natural := Token(Code, "entity");
            Port    : Natural := Token(Code, "port");
            End_Pos : Natural := Token(Code, "end");
            I       : Natural;  -- position of "is" in current line
            Name_s  : Natural;	-- start of a possible entity name
            Name_e  : Natural;  -- end of a possible entity name
            Name_n  : Natural;  -- start of next name (should be "is")
            Dot     : Natural;  -- position of "." indicating qualified name, e.g. entity instantiation

            procedure Get_Name is
            begin
               Name_e := Index(Code, Ada.Strings.Maps.To_Set(CharSet),
                                        Test => Ada.Strings.Outside, From => Name_s);
               if Name_e = 0 then Name_e := Code'last; end if;
               --Put_Line("Name : " & To_S(Name) & " "
               --           & natural'image(Name_s) & " " & natural'image(Name_e)
               --           & natural'image(Code'last));
               if Name_e < Code'last then
                  Name_n := Index(Code, Ada.Strings.Maps.To_Set(CharSet), From => Name_e);
               else
                  Name_n := 0;
               end if;
               I := Token(Code, "is", From => Name_e);
               Dot := Index(Code, ".", From => Name_e);

               if Name_e < Name_s then
                  Put_Line(Standard_Error, "Malformed name : " & Code);
               end if;
               Name := To_Unbounded_String (Code (Name_s .. Name_e-1));
               if I = 0 then  -- "is" must be on a subsequent line
                  State := Have_Name;
               elsif Name_n = I then   -- next word is "is"
                  State := In_Entity;
               elsif Dot < Name_n and Dot >= Name_e then
                  -- direct instantiation ... reject
                  State := Idle;
               elsif Name_n < I then
                  Put_Line(Standard_Error, "Name error : file " & Test_Name);
                  Put_Line(Standard_Error, "Entity : """ & Code(Name_s .. I-1) & """ not valid");
                  -- raise Program_Error;
               end if;
            end Get_Name;

         begin
            case State is
               when Idle =>
                  if Ent /= 0 then
                     -- Put_Line(Code);
                     Top_Level_Entity := True;
                     Name_s := Index(Code, Ada.Strings.Maps.To_Set(CharSet), From => Ent + 6);

                     if Name_s = 0 then -- entity name must be on a subsequent line
                        State := Have_Entity;
                     else
                        Get_Name;
                     end if;
                  end if;
               when Have_Entity =>
                  Name_s := Index(Code, Ada.Strings.Maps.To_Set(CharSet), From => Ent + 6);
                  if Name_s > 0 then
                     Get_Name;
                  end if;
               when Have_Name =>
                  if I > 0 then
                     State := In_Entity;
                  end if;
               when In_Entity =>  -- wait for End, handle Port;
                  -- NB the End may not be End Entity, but whatever it Ends, it must follow the port list
                  -- so we may stop looking for a port list when we see it.
                  if Port > 0 then
                     Top_Level_Entity := False;
                  end if;
                  if End_Pos > 0 then 
                     if Top_Level_Entity then -- write name to stdout
                        Last_Entity := Name;
                     end if;
                     State := Idle;
                  end if;
            end Case;
	    exit when End_Of_File (File);
         end;
      end loop;

      if Last_Entity /= "" then
         Put_Line (To_String (Last_Entity));
      end if;
      Close(File);

   end Get_Top_Entities;

   procedure Usage is
   begin
      Put_Line(Standard_Error,
               "Usage : " & Ada.Command_Line.Command_Name & " <filename>");
   end Usage;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      raise Program_Error;
   end if;
   Get_Top_Entities(Ada.Command_Line.Argument(1));
exception
   when Program_Error => Usage;
end get_entities;
