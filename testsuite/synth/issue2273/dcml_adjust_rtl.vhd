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
--         Filename:               dcml_adjust_rtl.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.4 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:44 $
--
--
--         Description: Combinational design to calculate the decimal
--                      representation (BCD) of a data bus.
--
--
--
--
-------------------------------------------------------------------------------
architecture rtl of dcml_adjust is

begin  -- rtl

  p_calc_adjst: process (data_i, cy_i)

    variable v_cy     : std_logic_vector((DWIDTH-1)/4 downto 0);
    variable v_nxtcy  : std_logic;
    variable v_tmpda  : unsigned(DWIDTH downto 0);
    variable v_tmpda1 : unsigned(4 downto 0);
    variable v_compvl : unsigned(3 downto 0);

  begin  -- process p_calc_adjst
    v_tmpda(DWIDTH-1 downto 0) := unsigned(data_i);
    v_tmpda(DWIDTH) := '0';
    v_cy := cy_i;
    v_nxtcy := '0';
    for i in 0 to (DWIDTH-1)/4 loop      
      if DWIDTH-i*4 <= 4 then
        -- Calculate the decimal adjustment of the last nibble/rest of bits
        v_compvl := conv_unsigned(0,4);
        v_compvl(DWIDTH-1-i*4 downto 0) := v_tmpda(DWIDTH-1 downto i*4);
        if (v_cy(i) = '1') or (v_compvl > conv_unsigned(9,4)) then
          if DWIDTH-i*4 > 2 then
            v_tmpda(DWIDTH downto i*4) := v_tmpda(DWIDTH-1 downto i*4) +
                       conv_unsigned(6,v_tmpda(DWIDTH downto i*4)'LENGTH);
          else
            v_tmpda(DWIDTH downto i*4) := v_tmpda(DWIDTH-1 downto i*4) +
                       conv_unsigned(2,v_tmpda(DWIDTH downto i*4)'LENGTH);
          end if;
        end if;
        -- An already set intermediate carry flag must not be lost.
        v_cy(i) := v_tmpda(DWIDTH) or v_cy(i);
      else
        -- Calculate the decimal adjustment of all nibbles, but the last one.
        v_compvl := v_tmpda(i*4+3 downto i*4);
        v_tmpda1 := conv_unsigned(0,5);
        if (v_cy(i) = '1') or (v_compvl > conv_unsigned(9,4)) then
          for j in i to (DWIDTH-1)/4 loop
            if DWIDTH-1 > j*4+3 then
              -- Calculate all subsequent nibbles from the actual position up
              -- to the one before the last.
              if j=i then
                v_tmpda1 := v_tmpda(j*4+3 downto j*4) +
                            conv_unsigned(6,5);
                v_nxtcy := v_tmpda1(4);
                v_tmpda(j*4+3 downto j*4) := v_tmpda1(3 downto 0);
              else
                v_tmpda1 := v_tmpda(j*4+3 downto j*4) +
                            conv_unsigned(v_nxtcy,5);
                v_nxtcy := v_tmpda1(4);
                v_tmpda(j*4+3 downto j*4) := v_tmpda1(3 downto 0);
              end if;
              -- An already set intermediate carry flag must not be lost.
              v_cy(j) := v_tmpda1(4) or v_cy(j);
            else
              -- Calculate the last nibble.
              if j=i then              
                v_tmpda(DWIDTH downto j*4) := v_tmpda(DWIDTH-1 downto j*4) +
                           conv_unsigned(6,v_tmpda(DWIDTH downto j*4)'LENGTH);
              else
                v_tmpda(DWIDTH downto j*4) := v_tmpda(DWIDTH-1 downto j*4) +
                     conv_unsigned(v_nxtcy,v_tmpda(DWIDTH downto j*4)'LENGTH);
              end if;
              -- An already set intermediate carry flag must not be lost.
              v_cy(j) := v_tmpda(DWIDTH) or v_cy(j);
            end if;
          end loop;  -- j
        end if;
      end if;
    end loop;  -- i
    -- Generate outputs
    cy_o <= v_cy(v_cy'HIGH);
    data_o <= std_logic_vector(v_tmpda(DWIDTH-1 downto 0));
  end process p_calc_adjst;

end rtl;
