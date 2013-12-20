
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: ch_20_ch_20_05.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_20_05 is

end entity ch_20_05;


----------------------------------------------------------------


architecture test of ch_20_05 is

  type stimulus_list is array (natural range <>) of integer;

  -- code from book:

  function "&" ( a, b : stimulus_list ) return stimulus_list;

  attribute debug : string;
  attribute debug of
    "&" [ stimulus_list, stimulus_list return stimulus_list ] : function is
    "source_statement_step";


    type mvl is ('X', '0', '1', 'Z');
    type mvl_vector is array ( integer range <>) of mvl;
    function resolve_mvl ( drivers : mvl_vector ) return mvl;

    subtype resolved_mvl is resolve_mvl mvl;


    type builtin_types is (builtin_bit, builtin_mvl, builtin_integer);
    attribute builtin : builtin_types;
    
    attribute builtin of resolved_mvl : subtype is builtin_mvl;

    -- end of code from book

    function "&" ( a, b : stimulus_list ) return stimulus_list is
    begin
      return stimulus_list'(1 to 0 => 0);
    end function "&";

    function resolve_mvl ( drivers : mvl_vector ) return mvl is
    begin
      return drivers(drivers'left);
    end function resolve_mvl;

  begin
  end architecture test;
