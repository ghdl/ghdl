
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
-- $Id: ch_16_fg_16_15.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity circuit is
  generic ( inpad_delay, outpad_delay : delay_length );
  port ( in1, in2, in3 : in bit;  out1, out2 : out bit );
end entity circuit;

--------------------------------------------------

architecture with_pad_delays of circuit is

  component subcircuit is
                         port ( a, b : in bit;  y1, y2 : out bit );
  end component subcircuit;

  signal delayed_in1, delayed_in2, delayed_in3 : bit;
  signal undelayed_out1, undelayed_out2 : bit;

begin

  input_delays : block is
  begin
    delayed_in1 <= in1 after inpad_delay;
    delayed_in2 <= in2 after inpad_delay;
    delayed_in3 <= in3 after inpad_delay;
  end block input_delays;

  functionality : block is
                          signal intermediate : bit;
  begin
    cell1 : component subcircuit
      port map ( delayed_in1, delayed_in2, undelayed_out1, intermediate );
    cell2 : component subcircuit
      port map ( intermediate, delayed_in3, undelayed_out2, open );
  end block functionality;

  output_delays : block is
  begin
    out1 <= undelayed_out1 after outpad_delay;
    out2 <= undelayed_out2 after outpad_delay;
  end block output_delays;

end architecture with_pad_delays;
