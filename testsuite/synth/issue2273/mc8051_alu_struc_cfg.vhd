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
--         Filename:               mc8051_alu_struc_cfg.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.6 $
--
--         Date of Latest Version: $Date: 2006-09-07 10:02:31 $
--
--
--         Description: Connects the units alumux, alucore, addsub_core,
--                      comb_mltplr, comb_divider, and dcml_adjust together.
--                      The whole design is made up of combinational logic.
--
--
--
--
-------------------------------------------------------------------------------
configuration mc8051_alu_struc_cfg of mc8051_alu is

  for struc
    for i_alumux : alumux
      use configuration work.alumux_rtl_cfg;
    end for;
    for i_alucore : alucore
      use configuration work.alucore_rtl_cfg;
    end for;
    for i_addsub_core : addsub_core
      use configuration work.addsub_core_struc_cfg;
    end for;
    for gen_multiplier1
      for all : comb_mltplr
        use configuration work.comb_mltplr_rtl_cfg;
      end for;
    end for;
    for gen_divider1
      for all : comb_divider
        use configuration work.comb_divider_rtl_cfg;
      end for;
    end for;
    for gen_dcml_adj1
      for all : dcml_adjust
        use configuration work.dcml_adjust_rtl_cfg;
      end for;
    end for;
  end for;
  
end mc8051_alu_struc_cfg;
