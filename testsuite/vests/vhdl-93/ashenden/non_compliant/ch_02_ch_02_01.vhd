
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
-- $Id: ch_02_ch_02_01.vhd,v 1.2 2001-10-26 16:29:37 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_02_01 is

end entity ch_02_01;


----------------------------------------------------------------


architecture test of ch_02_01 is
begin


  section_2_1_a : process is

                            -- code from book:

                            constant number_of_bytes : integer := 4;
                          constant number_of_bits : integer := 8 * number_of_bytes;
                          constant e : real := 2.718281828;
                          constant prop_delay : time := 3 ns;
                          constant size_limit, count_limit : integer := 255;

                          --

                          variable index : integer := 0;
                          variable sum, average, largest : real;
                          variable start, finish : time := 0 ns; 

                          -- end of code from book

  begin
    wait;
  end process section_2_1_a;


  ----------------


  section_2_1_b : process is

                            -- code from book:

                            variable start : time := 0 ns; 
                          variable finish : time := 0 ns; 

                          -- end of code from book

                          variable program_counter : integer;
                          variable index : integer;

  begin

    -- code from book:

    program_counter := 0;
    index := index + 1;

    -- end of code from book

    wait;
  end process section_2_1_b;


  ----------------


  section_2_2_a : process is

                            -- code from book:

                            type apples is range 0 to 100;
                          type oranges is range 0 to 100;

                          --

                          type day_of_month is range 0 to 31;
                          type year is range 0 to 2100;

                          variable today : day_of_month := 9;
                          variable start_year : year := 1987;

                          --

                          constant number_of_bits : integer := 32;
                          type bit_index is range 0 to number_of_bits - 1;

                          --

                          type set_index_range is range 21 downto 11;
                          type mode_pos_range is range 5 to 7;
                          variable set_index : set_index_range;
                          variable mode_pos : mode_pos_range;

                          --

                          type input_level is range -10.0 to +10.0;
                          type probability is range 0.0 to 1.0;

                          --

                          variable input_A : input_level;

                          -- end of code from book

  begin

    -- code from book:

    -- error: Incompatible types for assignment
    -- start_year := today;

    -- end of code from book

    wait;
  end process section_2_2_a;


  ----------------


  section_2_2_b : process is

                            -- code from book:

                            type resistance is range 0 to 1E9
                          units
                            ohm;
                          end units resistance;

                          -- end of code from book

  begin
    wait;
  end process section_2_2_b;


  ----------------


  section_2_2_c : process is

                            -- code from book:

                            type resistance is range 0 to 1E9
                          units
                            ohm;
                            kohm = 1000 ohm;
                            Mohm = 1000 kohm;
                          end units resistance;

                          -- end of code from book

  begin
    wait;
  end process section_2_2_c;


  ----------------


  section_2_2_d : process is

                            -- code from book:

                            type length is range 0 to 1E9
                          units
                            um;			-- primary unit: micron
                            mm = 1000 um;		-- metric units
                            m = 1000 mm;
                            mil = 254 um;		-- imperial units
                            inch = 1000 mil;
                          end units length;

                          -- end of code from book

  begin
    wait;
  end process section_2_2_d;


  ----------------


  section_2_2_e : process is

                            -- code from book:

                            -- type time is range implementation_defined
                            type time is range integer'low to integer'high
                          units
                            fs;
                            ps = 1000 fs;
                            ns = 1000 ps;
                            us = 1000 ns;
                            ms = 1000 us;
                            sec = 1000 ms;
                            min = 60 sec;
                            hr = 60 min;
                          end units;

                          -- end of code from book

  begin
    wait;
  end process section_2_2_e;


  ----------------


  section_2_2_f : process is

                            -- code from book:

                            type alu_function is (disable, pass, add, subtract, multiply, divide);

                          --

                          type octal_digit is ('0', '1', '2', '3', '4', '5', '6', '7');

                          --

                          variable alu_op : alu_function;
                          variable last_digit : octal_digit := '0';

                          --

                          type logic_level is (unknown, low, undriven, high);
                          variable control : logic_level;
                          type water_level is (dangerously_low, low, ok);
                          variable water_sensor : water_level;

                          -- end of code from book

  begin

    -- code from book:

    alu_op := subtract;
    last_digit := '7';

    --

    control := low;
    water_sensor := low;

    -- end of code from book

    wait;
  end process section_2_2_f;


  ----------------


  section_2_2_g : process is

                            -- code from book:

                            type severity_level is (note, warning, error, failure);

                          -- end of code from book

  begin
    wait;
  end process section_2_2_g;


  ----------------


  section_2_2_h : process is

                            -- code from book:

                            variable cmd_char, terminator : character;

                          -- end of code from book

  begin

    -- code from book:

    cmd_char := 'P';
    terminator := cr;

    -- end of code from book

    wait;
  end process section_2_2_h;


  ----------------


  section_2_2_i : process is

                            -- code from book:

                            type boolean is (false, true);

                          --

                          type bit is ('0', '1');

                          -- end of code from book

  begin
    wait;
  end process section_2_2_i;


  ----------------


  section_2_2_j : process is

                            variable write_enable_n, select_reg_n, write_reg_n : bit;

  begin

    -- code from book:

    write_reg_n := not ( not write_enable_n and not select_reg_n );

    -- end of code from book

    wait;
  end process section_2_2_j;


  ----------------


  section_2_2_k : process is

                            -- code from book:

                            type std_ulogic is ( 'U',	-- Uninitialized
                                                 'X',	-- Forcing Unknown
                                                 '0',	-- Forcing zero
                                                 '1',	-- Forcing one
                                                 'Z',	-- High Impedance
                                                 'W',	-- Weak Unknown
                                                 'L',	-- Weak zero
                                                 'H',	-- Weak one
                                                 '-' );	-- Don't care

                          -- end of code from book

  begin
    wait;
  end process section_2_2_k;


  ----------------


  section_2_3_a : process is

                            -- code from book:

                            subtype small_int is integer range -128 to 127;

                          --

                          variable deviation : small_int;
                          variable adjustment : integer;

                          --

                          subtype bit_index is integer range 31 downto 0;

                          -- end of code from book

  begin

    deviation := 0;
    adjustment := 0;

    -- code from book:

    deviation := deviation + adjustment;

    -- end of code from book

    wait;
  end process section_2_3_a;


  ----------------


  section_2_3_b : process is

                            constant highest_integer : integer := integer'high;

                          constant highest_time : time := time'high;

                          -- code from book:

                          subtype natural is integer range 0 to highest_integer;
                          subtype positive is integer range 1 to highest_integer;

                          --

                          subtype delay_length is time range 0 fs to highest_time;

                          -- end of code from book

  begin
    wait;
  end process section_2_3_b;


  ----------------


  section_2_3_c : process is

                            -- code from book:

                            type logic_level is (unknown, low, undriven, high);
                          type system_state is (unknown, ready, busy);

                          --

                          subtype valid_level is logic_level range low to high;

                          -- end of code from book

  begin
    wait;
  end process section_2_3_c;


  ----------------


  section_2_4_a : process is

                            -- code from book:

                            type resistance is range 0 to 1E9
                          units
                            ohm;
                            kohm = 1000 ohm;
                            Mohm = 1000 kohm;
                          end units resistance;

                          type set_index_range is range 21 downto 11;

                          type logic_level is (unknown, low, undriven, high);

                          -- end of code from book

  begin

    -- output from vsim: "2000"
    report resistance'image(2 kohm);

    -- code from book:

    assert resistance'left = 0 ohm;
    assert resistance'right = 1E9 ohm;
    assert resistance'low = 0 ohm;
    assert resistance'high = 1E9 ohm;
    assert resistance'ascending = true;
    assert resistance'image(2 kohm) = "2000 ohm";
    assert resistance'value("5 Mohm") = 5_000_000 ohm;

    assert set_index_range'left = 21;
    assert set_index_range'right = 11;
    assert set_index_range'low = 11;
    assert set_index_range'high = 21;
    assert set_index_range'ascending = false;
    assert set_index_range'image(14) = "14";
    assert set_index_range'value("20") = 20;

    assert logic_level'left = unknown;
    assert logic_level'right = high;
    assert logic_level'low = unknown;
    assert logic_level'high = high;
    assert logic_level'ascending = true;
    assert logic_level'image(undriven) = "undriven";
    assert logic_level'value("Low") = low;

    --

    assert logic_level'pos(unknown) = 0;
    assert logic_level'val(3) = high;
    assert logic_level'succ(unknown) = low;
    assert logic_level'pred(undriven) = low;

    --

    assert time'pos(4 ns) = 4_000_000;

    -- end of code from book

    wait;
  end process section_2_4_a;


  ----------------


  section_2_4_b : process is

                            -- code from book:

                            type length is range integer'low to integer'high
                          units
                            mm;
                          end units length;

                          type area is range integer'low to integer'high
                            units
                              square_mm;
                            end units area;

                          --

                          variable L1, L2 : length;
                          variable A : area;

                          -- end of code from book

  begin
    -- TG: avoid overflow in multiplication
    L1 := 1 mm;

    -- code from book:

    -- error: No feasible entries for infix op: "*"
    -- A := L1 * L2;      -- this is incorrect

    --

    A := area'val( length'pos(L1) * length'pos(L2) );

    -- end of code from book

    wait;
  end process section_2_4_b;


  ----------------


  section_2_4_c : process is

                            -- code from book:

                            type opcode is (nop, load, store, add, subtract, negate, branch, halt);
                          subtype arith_op is opcode range add to negate;

                          -- end of code from book

  begin

    -- code from book:

    assert arith_op'base'left = nop;
    assert arith_op'base'succ(negate) = branch;

    -- end of code from book

    wait;
  end process section_2_4_c;


end architecture test;
