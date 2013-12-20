
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
-- $Id: ch_13_fg_13_01.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity edge_triggered_Dff is
  generic ( Tprop, Tsetup, Thold : delay_length );
  port ( clk : in bit;  clr : in bit; d : in bit;
  q : out bit );
end entity edge_triggered_Dff;


architecture basic of edge_triggered_Dff is
begin

  state_change : process (clk, clr) is
  begin
    if clr = '1' then
      q <= '0' after Tprop;
    elsif clk'event and clk = '1' then
      q <= d after Tprop;
    end if;
  end process state_change;

end architecture basic;


architecture hi_fanout of edge_triggered_Dff is
begin

  state_change : process (clk, clr) is
  begin
    if clr = '1' then
      q <= '0' after Tprop;
    elsif clk'event and clk = '1' then
      q <= d after Tprop;
    end if;
  end process state_change;

end architecture hi_fanout;


-- code from book

entity reg4 is
  port ( clk, clr : in bit;  d : in bit_vector(0 to 3);
  q : out bit_vector(0 to 3) );
end entity reg4;

--------------------------------------------------

architecture struct of reg4 is

  component flipflop is
                       generic ( Tprop, Tsetup, Thold : delay_length );
                     port ( clk : in bit;  clr : in bit;  d : in bit;
                     q : out bit );
  end component flipflop;

begin

  bit0 : component flipflop
    generic map ( Tprop => 2 ns, Tsetup => 2 ns, Thold => 1 ns )
    port map ( clk => clk, clr => clr, d => d(0), q => q(0) );

  bit1 : component flipflop
    generic map ( Tprop => 2 ns, Tsetup => 2 ns, Thold => 1 ns )
    port map ( clk => clk, clr => clr, d => d(1), q => q(1) );

  bit2 : component flipflop
    generic map ( Tprop => 2 ns, Tsetup => 2 ns, Thold => 1 ns )
    port map ( clk => clk, clr => clr, d => d(2), q => q(2) );

  bit3 : component flipflop
    generic map ( Tprop => 2 ns, Tsetup => 2 ns, Thold => 1 ns )
    port map ( clk => clk, clr => clr, d => d(3), q => q(3) );

end architecture struct;

-- end code from book



configuration fg_13_01 of reg4 is

  for struct

    -- code from book (in text)

    for bit0, bit1 : flipflop
      use entity work.edge_triggered_Dff(basic);
    end for;

    -- end code from book

  end for;

end configuration fg_13_01;
