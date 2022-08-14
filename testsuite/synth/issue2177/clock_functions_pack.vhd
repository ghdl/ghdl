----------------------------------------------------------------------
--               Typedefs and functions for clocks
--                      www.fpgaarcade.com
--                     All rights reserved.
--
--                     admin@fpgaarcade.com
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
----------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package clock_functions_pack is

  -----------------------------------------------------------------------------
  -- Transparent handling of clock enables
  --
  -- Basic type def
  type r_clk is record
    base : std_logic;
    val  : std_logic;
    rise : std_logic;
    fall : std_logic;
  end record;
  constant z_clk : r_clk := (
    base => '0',
    val  => '0',
    rise => '0',
    fall => '0');

  -- Overloaded functions for synchronous process template
  function rising_edge(signal clk : r_clk) return boolean;
  function falling_edge(signal clk : r_clk) return boolean;

  -- Overloaded functions for boolean arithmetic on clocks
  function  "not" (clk : r_clk) return r_clk;
  --
  function  "and" (clk1, clk2 : r_clk) return r_clk;
  function  "and" (clk1 : r_clk; op2 : std_logic) return r_clk;
  function  "and" (op1 : std_logic; clk2 : r_clk) return std_logic;
  --
  function "nand" (clk1, clk2 : r_clk) return r_clk;
  function "nand" (clk1 : r_clk; op2 : std_logic) return r_clk;
  function "nand" (op1 : std_logic; clk2 : r_clk) return std_logic;
  --
  function   "or" (clk1, clk2 : r_clk) return r_clk;
  function   "or" (clk1 : r_clk; op2 : std_logic) return r_clk;
  function   "or" (op1 : std_logic; clk2 : r_clk) return std_logic;
  --
  function  "nor" (clk1, clk2 : r_clk) return r_clk;
  function  "nor" (clk1 : r_clk; op2 : std_logic) return r_clk;
  function  "nor" (op1 : std_logic; clk2 : r_clk) return std_logic;
  --
  function    "=" (clk1 : r_clk; op2 : std_logic) return boolean;

end;

package body clock_functions_pack is

  function rising_edge(signal clk : r_clk) return boolean is
  begin
    return rising_edge(clk.base) and clk.rise = '1';
  end;

  function falling_edge(signal clk : r_clk) return boolean is
  begin
    return rising_edge(clk.base) and clk.fall = '1';
  end;

  function  "not" (clk : r_clk) return r_clk is
  begin
    return (base => clk.base,
            val  => not clk.val,
            rise => clk.fall,
            fall => clk.rise);
  end;

  function  "and" (clk1, clk2 : r_clk) return r_clk is
  begin
    return (base => clk1.base,
            val  => clk1.val and clk2.val,
            rise => (clk1.rise and clk2.val and not clk2.fall) or
                    (clk2.rise and clk1.val and not clk1.fall),
            fall => (clk1.fall and clk2.val) or
                    (clk2.fall and clk1.val));
  end;

  function  "and" (clk1 : r_clk; op2 : std_logic) return r_clk is
  begin
    return (base => clk1.base,
            val  => clk1.val and op2,
            rise => clk1.rise and op2,
            fall => clk1.fall and op2);
  end;

  function  "and" (op1 : std_logic; clk2 : r_clk) return std_logic is
  begin
    return op1 and clk2.val;
  end;

  function "nand" (clk1, clk2 : r_clk) return r_clk is
  begin
    return (base => clk1.base,
            val  => clk1.val nand clk2.val,
            rise => (clk1.fall and clk2.val) or
                    (clk2.fall and clk1.val),
            fall => (clk1.rise and clk2.val and not clk2.fall) or
                    (clk2.rise and clk1.val and not clk1.fall));
  end;

  function "nand" (clk1 : r_clk; op2 : std_logic) return r_clk is
  begin
    return (base => clk1.base,
            val  => clk1.val nand op2,
            rise => clk1.fall and op2,
            fall => clk1.rise and op2);
  end;

  function "nand" (op1 : std_logic; clk2 : r_clk) return std_logic is
  begin
    return op1 nand clk2.val;
  end;

  function   "or" (clk1, clk2 : r_clk) return r_clk is
  begin
    return (base => clk1.base,
            val  => clk1.val or clk2.val,
            rise => (clk1.rise and not clk2.val) or
                    (clk2.rise and not clk1.val),
            fall => (clk1.fall and not clk2.val and not clk2.rise) or
                    (clk2.fall and not clk1.val and not clk1.rise));
  end;

  function   "or" (clk1 : r_clk; op2 : std_logic) return r_clk is
  begin
    return (base => clk1.base,
            val  => clk1.val or op2,
            rise => clk1.rise and not op2,
            fall => clk1.fall and not op2);
  end;

  function   "or" (op1 : std_logic; clk2 : r_clk) return std_logic is
  begin
    return op1 or clk2.val;
  end;

  function  "nor" (clk1, clk2 : r_clk) return r_clk is
  begin
    return (base => clk1.base,
            val  => clk1.val nor clk2.val,
            rise => (clk1.fall and not clk2.val and not clk2.rise) or
                    (clk2.fall and not clk1.val and not clk1.rise),
            fall => (clk1.rise and not clk2.val) or
                    (clk2.rise and not clk1.val));
  end;

  function  "nor" (clk1 : r_clk; op2 : std_logic) return r_clk is
  begin
    return (base => clk1.base,
            val  => clk1.val nor op2,
            rise => clk1.fall and not op2,
            fall => clk1.rise and not op2);
  end;

  function  "nor" (op1 : std_logic; clk2 : r_clk) return std_logic is
  begin
    return op1 nor clk2.val;
  end;

  function    "=" (clk1 : r_clk; op2 : std_logic) return boolean is
  begin
    return clk1.val = op2;
  end;

end;
