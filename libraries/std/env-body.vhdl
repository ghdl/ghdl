--  Std.Env package declaration.  This file is part of GHDL.
--  This file was written from the clause 14.3 of the VHDL LRM.
--  Copyright (C) 2014 Tristan Gingold
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

package body Env is
  procedure control_simulation (Is_Stop : Boolean;
                                Has_Status : Boolean;
                                Status : Integer);
  attribute foreign of control_simulation : procedure is "GHDL intrinsic";

  procedure control_simulation (Is_Stop : Boolean;
                                Has_Status : Boolean;
                                Status : Integer) is
  begin
    assert false report "must not be called" severity failure;
  end control_simulation;

  procedure Stop (Status : Integer) is
  begin
    control_simulation (True, True, Status);
  end Stop;

  procedure Stop is
  begin
    control_simulation (True, False, -1);
  end Stop;

  procedure Finish (status : integer) is
  begin
    control_simulation (False, True, Status);
  end Finish;

  procedure Finish is
  begin
    control_simulation (False, False, -1);
  end Finish;

  function Get_Resolution_Limit return Delay_Length;
  attribute foreign of Get_Resolution_Limit : function is "GHDL intrinsic";

  function Get_Resolution_Limit return Delay_Length is
  begin
    assert false report "must not be called" severity failure;
  end Get_Resolution_Limit;

  function Resolution_Limit return Delay_Length is
  begin
    return Get_Resolution_Limit;
  end Resolution_Limit;

--START-V19
  function Get_Assert_Count (Level : Severity_Level) return Natural;
  attribute foreign of Get_Assert_Count : function is "GHDL intrinsic";

  function Get_Assert_Count (Level : Severity_Level) return Natural is
  begin
    assert false report "must not be called" severity failure;
    return 0;
  end Get_Assert_Count;

  procedure Clear_Assert_Count;
  attribute foreign of Clear_Assert_Count : procedure is "GHDL intrinsic";

  procedure Clear_Assert_Count is
  begin
    assert false report "must not be called" severity failure;
  end Clear_Assert_Count;

  -- VHDL Assert Count
  impure function GetVhdlAssertCount return natural
  is
    variable Res : Natural;
  begin
    for I in Warning to Error loop
      Res := Res + Get_Assert_Count (I);
    end loop;
    return Res;
  end GetVhdlAssertCount;

  impure function GetVhdlAssertCount (Level : SEVERITY_LEVEL ) return natural is
  begin
    return Get_Assert_Count (Level);
  end GetVhdlAssertCount;

  procedure ClearVhdlAssert is
  begin
    Clear_Assert_Count;
  end ClearVhdlAssert;
--END-V19
end package body Env;
