
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

entity processor is
end entity processor;



-- code from book

architecture rtl of processor is

  subtype word is bit_vector(0 to 31);
  type word_vector is array (natural range <>) of word;

  function resolve_unique ( drivers : word_vector ) return word is
  begin
    return drivers(drivers'left);
  end function resolve_unique;

  signal source1, source2 : resolve_unique word register;
  -- . . .

  -- not in book

  type alu_op_type is (pass1, pass2, add, subtract);

  procedure perform_alu_op ( signal alu_opcode : in alu_op_type;
			     signal source1, source2 : in word;
			     signal destination : out word;
			     constant ignored : in integer := 0 ) is
  begin
    null;
  end procedure perform_alu_op;

  signal phase1, source1_reg_out_en,other_signal : bit;
  signal alu_opcode : alu_op_type;
  signal destination : word;

  -- end not in book

begin

  source1_reg : process (phase1, source1_reg_out_en, -- . . .) is
    -- not in book
      other_signal) is
    -- end not in book
    variable stored_value : word;
  begin
    -- . . .
    if source1_reg_out_en = '1' and phase1 = '1' then
      source1 <= stored_value;
      -- not in book
      stored_value := not stored_value;
      -- end not in book
    else
      source1 <= null;
    end if;
  end process source1_reg;

  alu : perform_alu_op ( alu_opcode, source1, source2, destination, -- . . . );
    -- not in book
      open );
    -- end not in book

  -- . . .

  -- not in book

  process is
  begin
    wait for 10 ns;
    source1_reg_out_en <= '1';
    phase1 <= '1', '0' after 10 ns;
    wait for 20 ns;
    source1_reg_out_en <= '1';
    phase1 <= '1', '0' after 10 ns;
    wait;
  end process;

  -- end not in book

end architecture rtl;

-- end code from book
