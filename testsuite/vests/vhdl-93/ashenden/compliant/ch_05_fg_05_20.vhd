
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
-- $Id: ch_05_fg_05_20.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_05_20 is
end entity fg_05_20;


architecture test of fg_05_20 is

  constant Tpd : delay_length := 2 ns;

  function "+" ( bv1, bv2 : in bit_vector ) return bit_vector is

    alias op1 : bit_vector(1 to bv1'length) is bv1;
    alias op2 : bit_vector(1 to bv2'length) is bv2;  
    variable result : bit_vector(1 to bv1'length);
    variable carry_in : bit;
    variable carry_out : bit := '0';

  begin
    for index in result'reverse_range loop
      carry_in := carry_out;  -- of previous bit
      result(index) := op1(index) xor op2(index) xor carry_in;
      carry_out := (op1(index) and op2(index))
      	      	   or (carry_in and (op1(index) xor op2(index)));
    end loop;
    return result;
  end function "+";

  function "-" ( bv1, bv2 : in bit_vector ) return bit_vector is

    -- subtraction implemented by adding ((not bv2) + 1), ie -bv2

    alias op1 : bit_vector(1 to bv1'length) is bv1;
    alias op2 : bit_vector(1 to bv2'length) is bv2;  
    variable result : bit_vector(1 to bv1'length);
    variable carry_in : bit;
    variable carry_out : bit := '1';

  begin
    for index in result'reverse_range loop
      carry_in := carry_out;  -- of previous bit
      result(index) := op1(index) xor (not op2(index)) xor carry_in;
      carry_out := (op1(index) and (not op2(index)))
      	      	   or (carry_in and (op1(index) xor (not op2(index))));
    end loop;
    return result;
  end function "-";

  type alu_function_type is (alu_pass_a, alu_add, alu_sub,
			     alu_add_unsigned, alu_sub_unsigned,
			     alu_and, alu_or);

  signal alu_function : alu_function_type := alu_pass_a;
  signal a, b : bit_vector(15 downto 0);
  signal functional_result, equivalent_result : bit_vector(15 downto 0);

begin

  functional_alu : block is
                           port ( result : out bit_vector(15 downto 0) );
                         port map ( result => functional_result );
  begin

    -- code from book

    alu : with alu_function select
      result <=  a + b after Tpd   when alu_add | alu_add_unsigned,
      a - b after Tpd   when alu_sub | alu_sub_unsigned,
      a and b after Tpd when alu_and,
      a or b after Tpd  when alu_or,
      a after Tpd       when alu_pass_a;

    -- end code from book

  end block functional_alu;

  --------------------------------------------------
  
  equivalent_alu : block is
                           port ( result : out bit_vector(15 downto 0) );
                         port map ( result => equivalent_result );
  begin

    -- code from book

    alu : process is
    begin
      case alu_function is
        when alu_add | alu_add_unsigned =>  result <= a + b after Tpd;
        when alu_sub | alu_sub_unsigned =>  result <= a - b after Tpd;
        when alu_and                    =>  result <= a and b after Tpd;
        when alu_or                     =>  result <= a or b after Tpd;
        when alu_pass_a                 =>  result <= a after Tpd;
      end case;
      wait on alu_function, a, b;
    end process alu;

    -- end code from book

  end block equivalent_alu;

  --------------------------------------------------

  stimulus : process is
  begin
    alu_function <= alu_add;	wait for 10 ns;
    a <= X"000A";		wait for 10 ns;
    b <= X"0003";		wait for 10 ns;
    alu_function <= alu_sub;	wait for 10 ns;
    alu_function <= alu_and;	wait for 10 ns;
    alu_function <= alu_or;	wait for 10 ns;
    alu_function <= alu_pass_a;	wait for 10 ns;

    wait;
  end process stimulus;

  verifier :
    assert functional_result = equivalent_result
      report "Functional and equivalent models give different results";

end architecture test;
