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
--         Filename:               addsub_ovcy_rtl.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.4 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:44 $
--
--
--         Description: Adder/Subtractor with carry/borrow and arbitrary data
--                      width and overflow flag.
--
--
--
--
-------------------------------------------------------------------------------
architecture rtl of addsub_ovcy is

begin

  gen_equal_one: if (DWIDTH = 1) generate
    -- purpose: Simple adder/subtractor with carry/borrow and overflow
    -- type   : combinational
    -- inputs : opa_i, opb_i, addsub_i, cy_i
    -- outputs: cy_o, rslt_o
    p_addsub_ov: process (opa_i, opb_i, addsub_i, cy_i)
      variable v_la : unsigned(1 downto 0);
      variable v_lb : unsigned(1 downto 0);
      variable v_lresult : std_logic_vector(2 downto 0);
    begin  -- process p_addsub
      v_la(1) := opa_i(DWIDTH-1);
      v_lb(1) := opb_i(DWIDTH-1);
      if addsub_i = '1' then
        v_la(0) := '1';
        v_lb(0) := cy_i;
        v_lresult := conv_unsigned(v_la,3) + unsigned(v_lb);
      else
        v_la(0) := '0';
        v_lb(0) := cy_i;
        v_lresult := conv_unsigned(v_la,3) - unsigned(v_lb);
      end if;
      cy_o <= v_lresult(2);
      ov_o <= (cy_i and not(v_lresult(2))) or
  	    (v_lresult(2) and not(cy_i));
      rslt_o(DWIDTH-1) <= v_lresult(1);
    end process p_addsub_ov;
  end generate gen_equal_one;

  gen_greater_one: if (DWIDTH > 1) generate
    -- purpose: Simple adder/subtractor with carry/borrow and overflow
    -- type   : combinational
    -- inputs : opa_i, opb_i, addsub_i, cy_i
    -- outputs: cy_o, rslt_o
    p_addsub_ov: process (opa_i, opb_i, addsub_i, cy_i)
      variable v_a : unsigned(DWIDTH-1 downto 0);
      variable v_b : unsigned(DWIDTH-1 downto 0);
      variable v_result : std_logic_vector(DWIDTH downto 0);
      variable v_la : unsigned(1 downto 0);
      variable v_lb : unsigned(1 downto 0);
      variable v_lresult : std_logic_vector(2 downto 0);
    begin  -- process p_addsub
      v_a(DWIDTH-1 downto 1) := unsigned(opa_i(DWIDTH-2 downto 0));
      v_b(DWIDTH-1 downto 1) := unsigned(opb_i(DWIDTH-2 downto 0));
      v_la(1) := opa_i(DWIDTH-1);
      v_lb(1) := opb_i(DWIDTH-1);
      if addsub_i = '1' then
        v_a(0) := '1';
        v_b(0) := cy_i;
        v_result := conv_unsigned(v_a,DWIDTH+1) + unsigned(v_b);
        v_la(0) := '1';
        v_lb(0) := v_result(DWIDTH);
        v_lresult := conv_unsigned(v_la,3) + unsigned(v_lb);
      else
        v_a(0) := '0';
        v_b(0) := cy_i;
        v_result := conv_unsigned(v_a,DWIDTH+1) - unsigned(v_b);
        v_la(0) := '0';
        v_lb(0) := v_result(DWIDTH);      
        v_lresult := conv_unsigned(v_la,3) - unsigned(v_lb);
      end if;
      cy_o <= v_lresult(2);
      ov_o <= (v_result(DWIDTH) and not(v_lresult(2))) or
  	    (v_lresult(2) and not(v_result(DWIDTH)));
      rslt_o(DWIDTH-2 downto 0) <= v_result(DWIDTH-1 downto 1);
      rslt_o(DWIDTH-1) <= v_lresult(1);
    end process p_addsub_ov;
  end generate gen_greater_one;

end rtl;
