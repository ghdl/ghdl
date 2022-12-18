-------------------------------------------------------------------------------
--                                                                           --
--          X       X   XXXXXX    XXXXXX    XXXXXX    XXXXXX      X          --
--          XX     XX  X      X  X      X  X      X  X           XX          --
--          X X   X X  X         X      X  X      X  X          X X          --
--          X  X X  X  X         X      X  X      X  X         X  X          --
--          X   X   X  X          XXXXXX   X      X   XXXXXX      X          --
--          X       X  X         X      X  X      X         X     X          --
--          X       X  X         X      X  X      X         X     X          --
--          X       X  X      X  X      X  X      X         X     X          --
--          X       X   XXXXXX    XXXXXX    XXXXXX    XXXXXX      X          --
--                                                                           --
--                                                                           --
--                       O R E G A N O   S Y S T E M S                       --
--                                                                           --
--                            Design & Consulting                            --
--                                                                           --
-------------------------------------------------------------------------------
--                                                                           --
--         Web:           http://www.oregano.at/                             --
--                                                                           --
--         Contact:       mc8051@oregano.at                                  --
--                                                                           --
-------------------------------------------------------------------------------
--                                                                           --
--  MC8051 - VHDL 8051 Microcontroller IP Core                               --
--  Copyright (C) 2001 OREGANO SYSTEMS                                       --
--                                                                           --
--  This library is free software; you can redistribute it and/or            --
--  modify it under the terms of the GNU Lesser General Public               --
--  License as published by the Free Software Foundation; either             --
--  version 2.1 of the License, or (at your option) any later version.       --
--                                                                           --
--  This library is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        --
--  Lesser General Public License for more details.                          --
--                                                                           --
--  Full details of the license can be found in the file LGPL.TXT.           --
--                                                                           --
--  You should have received a copy of the GNU Lesser General Public         --
--  License along with this library; if not, write to the Free Software      --
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA  --
--                                                                           --
-------------------------------------------------------------------------------
--
--
--         Author:                 Roland Höller
--
--         Filename:               addsub_core_struc.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.4 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:44 $
--
--
--         Description: Adder/Subtractor with carry/borrow, arbitrary data 
--                      width, overflow, and nibble carry for decimal 
--                      adjustment.
--
--
--
--
-------------------------------------------------------------------------------
architecture struc of addsub_core is

  type t_cy is array (1 to (DWIDTH/4)+1) of std_logic_vector(0 downto 0);

  signal s_cy : t_cy;

begin  -- architecture structural

  gen_smorequ_four : if (DWIDTH > 0 and DWIDTH <= 4) generate
    addsub_ovcy_1 : addsub_ovcy
      generic map (DWIDTH => DWIDTH)
      port map (opa_i    => opa_i,
                opb_i    => opb_i,
                addsub_i => addsub_i,
                cy_i     => cy_i,
                cy_o     => cy_o(0),
                ov_o     => ov_o,
                rslt_o   => rslt_o);
  end generate gen_smorequ_four;

  s_cy(1)(0) <= cy_i;
  
  gen_greater_four : if (DWIDTH > 4) generate
    gen_addsub: for i in 1 to DWIDTH generate
      gen_nibble_addsub: if (i mod 4 = 0) and i <= ((DWIDTH-1)/4)*4 generate
	i_addsub_cy: addsub_cy
	  generic map (DWIDTH => 4)
	  port map (opa_i => opa_i(i-1 downto i-4),
		    opb_i => opb_i(i-1 downto i-4),
		    addsub_i => addsub_i,
		    cy_i => s_cy(i/4)(0),
		    cy_o => s_cy((i+4)/4)(0),
		    rslt_o => rslt_o(i-1 downto i-4));
	cy_o(i/4-1) <= s_cy((i+4)/4)(0);
      end generate gen_nibble_addsub;
      gen_last_addsub: if (i = ((DWIDTH-1)/4)*4+1) generate
	i_addsub_ovcy: addsub_ovcy
	  generic map (DWIDTH => DWIDTH-((DWIDTH-1)/4)*4)
	  port map (opa_i => opa_i(DWIDTH-1 downto i-1), 
		    opb_i => opb_i(DWIDTH-1 downto i-1), 
		    addsub_i => addsub_i,
		    cy_i => s_cy((DWIDTH-1)/4+1)(0),
		    cy_o => cy_o((DWIDTH-1)/4),
		    ov_o => ov_o,
		    rslt_o => rslt_o(DWIDTH-1 downto i-1)); 
      end generate gen_last_addsub;
    end generate gen_addsub;
  end generate gen_greater_four;

end struc;
