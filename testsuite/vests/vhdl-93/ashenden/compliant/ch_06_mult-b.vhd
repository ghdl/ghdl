
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
-- $Id: ch_06_mult-b.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture behavioral of multiplier is
begin

  behavior : process (a, b) is

                              constant Tpd_in_out : time := 40 ns;
                            variable negative_result  : boolean;
                            variable op1 : std_ulogic_vector(15 downto 0);
                            variable op2 : std_ulogic_vector(15 downto 0);
                            variable result : std_ulogic_vector(31 downto 0);
                            variable carry_in, carry : std_ulogic;

  begin
    op1 := to_X01(a);
    op2 := to_X01(b);
    -- make both operands positive, remembering sign of result
    negative_result := (op1(15) = '1') xor (op2(15) = '1');
    if (op1(15) = '1') then
      carry := '1';
      for index in 0 to 15 loop
        carry_in := carry;
        carry := carry_in and not op1(index);
        op1(index) := not op1(index) xor carry_in;
      end loop;
    end if;
    if (op2(15) = '1') then
      carry := '1';
      for index in 0 to 15 loop
        carry_in := carry;
        carry := carry_in and not op2(index);
        op2(index) := not op2(index) xor carry_in;
      end loop;
    end if;
    -- do long multiplication
    result := (others => '0');
    for count in 0 to 15 loop
      carry := '0';
      if (op2(count) = '1') then
        for index in 0 to 15 loop
          carry_in := carry;
          carry := (result(index+count) and op1(index))
                   or (carry_in and (result(index+count) xor op1(index)));
          result(index+count) := result(index+count) xor op1(index) xor carry_in;
        end loop;
        result(count+16) := carry;
      end if;
    end loop;
    -- result now contains unsigned product, with binary point
    -- between bits 30 and 29.  assign output with sign adjusted.
    if negative_result then
      carry := '1';
      for index in 0 to 31 loop
        carry_in := carry;
        carry := carry_in and not result(index);
        result(index) := not result(index) xor carry_in;
      end loop;
    end if;
    p <= result after Tpd_in_out;
  end process behavior;

end architecture behavioral;

