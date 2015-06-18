--========================================================================================================================
-- Copyright (c) 2015 by Bitvis AS.  All rights reserved.
-- A free license is hereby granted, free of charge, to any person obtaining
-- a copy of this VHDL code and associated documentation files (for 'Bitvis Utility Library'),
-- to use, copy, modify, merge, publish and/or distribute - subject to the following conditions:
--  - This copyright notice shall be included as is in all copies or substantial portions of the code and documentation
--  - The files included in Bitvis Utility Library may only be used as a part of this library as a whole
--  - The License file may not be modified
--  - The calls in the code to the license file ('show_license') may not be removed or modified.
--  - No other conditions whatsoever may be added to those of this License

-- BITVIS UTILITY LIBRARY AND ANY PART THEREOF ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
-- INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH BITVIS UTILITY LIBRARY.
--========================================================================================================================

------------------------------------------------------------------------------------------
-- VHDL unit     : Bitvis Utility Library : license_pkg
--
-- Description   : See library quick reference (under 'doc') and README-file(s)
------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;

use work.types_pkg.all;
use work.string_methods_pkg.all;
use work.adaptations_pkg.all;

package license_pkg is

  impure function show_license(
    constant dummy  : in t_void
    ) return boolean;

  impure function show_bitvis_utility_library_info(
    constant dummy  : in t_void
    ) return boolean;

  impure function show_bitvis_utility_library_release_info(
    constant dummy  : in t_void
    ) return boolean;

end package license_pkg;

package body license_pkg is



  impure function show_license(
    constant dummy  : in t_void
    ) return boolean is
    constant C_VERSION            : string  := "v2.5.1"; -- June 2015
    constant C_SEPARATOR          : string  :=
      "=====================================================================================================";

    constant C_LICENSE_STR        : string :=
      LF & LF & LF &
      C_SEPARATOR                                                                                             & LF &
      C_SEPARATOR                                                                                             & LF &
      " Bitvis Utility Library " & C_VERSION & " is being used by this simulation."                                             & LF &
      " This is a *** LICENSED PRODUCT *** as given in the copyright notice of the VHDL code."                & LF &
      " The free license granted is subject to the conditions given in the VHDL copyright notice."            & LF &
      C_SEPARATOR                                                                                             & LF &
      C_SEPARATOR                                                                                             & LF & LF;

  begin
    report (C_LICENSE_STR);
    return true;
  end;

  impure function show_bitvis_utility_library_info(
    constant dummy  : in t_void
    ) return boolean is
    constant C_SEPARATOR          : string  :=
      "=====================================================================================================";

    constant C_LICENSE_STR        : string :=
      LF & LF &
      C_SEPARATOR                                                                                             & LF &
      C_SEPARATOR & LF &
      "This info section may be turned off via C_SHOW_BITVIS_UTILITY_LIBRARY_INFO in adaptations_pkg.vhd"  & LF & LF &
      "Important Simulator setup: " & LF &
      "- Set simulator to break on severity 'FAILURE' " & LF &
      "- Set simulator transcript to a monospace font (e.g. Courier new)" & LF & LF &
      "Bitvis Utility Library setup:" & LF &
      "- It is recommended to go through the two powerpoint presentations provided with the download" & LF &
      "- There is a Quick-Reference in the doc-directory" & LF &
      "- In order to change layout or behaviour - please check the src*/adaptations_pkg.vhd" & LF &
      "  This is intended for personal or company customization" & LF & LF &
      "License conditions are given in License.txt" & LF &
      C_SEPARATOR                                                                                             & LF &
      C_SEPARATOR                                                                                             & LF & LF;

  begin
    if C_SHOW_BITVIS_UTILITY_LIBRARY_INFO then
      report (C_LICENSE_STR);
    end if;
    return true;
  end;


  impure function show_bitvis_utility_library_release_info(
    constant dummy  : in t_void
    ) return boolean is
    constant C_IMPORTANT_UPDATE_FOR_THIS_VERSION : boolean := true;   -- ***** NOTE: Evaluate a change here
    constant C_SEPARATOR          : string  :=
      "=====================================================================================================";

    constant C_LICENSE_STR        : string :=
      LF & LF &
      C_SEPARATOR                                                                                             & LF &
      C_SEPARATOR & LF &
      "This release info may be turned off via C_SHOW_BITVIS_UTILITY_LIBRARY_RELEASE_INFO in adaptations_pkg.vhd"  & LF & LF &
      "Important Issues for this version update: " & LF &
      "- Two procedures have changed name (see CHANGES.TXT)" & LF &
      "  The old names will still work for a few more version updates, but a deprecate-message will be displayed" & LF &
      "  (The deprecate-message may be turned off, but we recommend to rather change the actual procedure names)" & LF & LF &
      C_SEPARATOR                                                                                             & LF &
      C_SEPARATOR                                                                                             & LF & LF;

  begin
    if  C_SHOW_BITVIS_UTILITY_LIBRARY_RELEASE_INFO and C_IMPORTANT_UPDATE_FOR_THIS_VERSION then
      report (C_LICENSE_STR);
    end if;
    return true;
  end;


end package body license_pkg;

