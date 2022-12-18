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
--         Filename:               comb_divider_rtl.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.4 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:44 $
--
--
--         Description: Divider with parameteriseable data width. Realised
--                      using combinational logic only.
--
--
--
--
-------------------------------------------------------------------------------
architecture rtl of comb_divider is

begin  -- rtl

  -- purpose: Divide dvdnd_i through dvsor_i and deliver the result to qutnt_o
  --          and the remainder to rmndr_o.
  -- type   : combinational
  -- inputs : dvdnd_i, dvsor_i
  -- outputs: qutnt_o, rmndr_o
  p_divide: process (dvdnd_i, dvsor_i)

    variable v_actl_dvdnd : unsigned(DWIDTH-1 downto 0);
    variable v_dffrnc     : unsigned(DWIDTH-1 downto 0);
    variable v_qutnt      : unsigned(DWIDTH-1 downto 0);
    
  begin  -- process p_divide

    v_actl_dvdnd := unsigned(dvdnd_i);
    
    for i in DWIDTH-1 downto 0 loop
      -- If the divisor can be subtracted from this part of the dividend, then
      -- the corresponding bit of the quotient has to be 1, otherwise 0.
      if conv_std_logic_vector(v_actl_dvdnd(DWIDTH-1 downto i),DWIDTH) >=
        dvsor_i then
        -- Divisor can be subtracted
        v_qutnt(i) := '1';
        v_dffrnc := conv_unsigned(v_actl_dvdnd(DWIDTH-1 downto i),DWIDTH)
                    - unsigned(dvsor_i);
        -- As long as this is not the last step of calculation, shift the
        -- intermediate result.
        if i /= 0 then
          v_actl_dvdnd(DWIDTH-1 downto i) := v_dffrnc(DWIDTH-1-i downto 0);
          v_actl_dvdnd(i-1) := dvdnd_i(i-1);
        end if;
      else
        -- Divisor is greater than this part of the dividend.
        v_qutnt(i) := '0';
        v_dffrnc := conv_unsigned(v_actl_dvdnd(DWIDTH-1 downto i),DWIDTH);
      end if;
    end loop;  -- i
    
    rmndr_o <= std_logic_vector(v_dffrnc);
    qutnt_o <= std_logic_vector(v_qutnt);
    
  end process p_divide;

end rtl;


