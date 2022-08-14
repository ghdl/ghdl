----------------------------------------------------------------------
--                           VLM5030
--                      www.fpgaarcade.com
--                     All rights reserved.
--
--                     admin@fpgaarcade.com
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
----------------------------------------------------------------------
--
-- Copyright (c) 2021, Arnim Laeuger  arnim.laeuger@gmx.net
-- All rights reserved.
--


-------------------------------------------------------------------------------
-- SR-latch, synchronous to common clock
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity vlm5030_srlatch is

  port (
    i_clk : in  std_logic;
    i_res : in  std_logic;
    i_set : in  std_logic;
    o_q   : out std_logic
  );

end;

architecture rtl of vlm5030_srlatch is
  signal q : std_logic := '0';
begin

  process (i_clk)
  begin
    if rising_edge(i_clk) then
      if i_res = '1' then
        q <= '0';
      elsif i_set = '1' then
        q <= '1';
      end if;
    end if;
  end process;

  o_q <= q;

end;


-------------------------------------------------------------------------------
-- SR-latch, synchronous to common clock, r_clk version
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.clock_functions_pack.all;

entity vlm5030_srlatchclk is

  port (
    i_clk : in  r_clk;
    i_res : in  r_clk;
    i_set : in  r_clk;
    o_q   : out r_clk
  );

end;

architecture rtl of vlm5030_srlatchclk is
  signal q : std_logic := '0';
begin

  process (i_clk)
  begin
    if rising_edge(i_clk) then
      if i_res.val = '1' then
        q <= '0';
      elsif i_set.val = '1' then
        q <= '1';
      end if;
    end if;
  end process;

  o_q <= (base => i_clk.base,
          val  => q,
          rise => not q and i_set.val,
          fall =>     q and i_res.val);

end;


-------------------------------------------------------------------------------
-- vlm5030_delay
--
-- Delay input signal by the specified number of clocks.
--
-- NOTE: This is inertial delay.
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.clock_functions_pack.all;

entity vlm5030_delay is

  generic (
    g_numclks : integer := 3
  );
  port (
    i_clk : in  r_clk;
    i_in  : in  std_logic;
    o_out : out std_logic
  );

end;

architecture rtl of vlm5030_delay is
begin

  delay_p : process (i_clk)
    variable cnt : natural := 0;
    variable inq : std_logic := '0';
  begin
    if rising_edge(i_clk) then
      if i_in /= inq then
        cnt := g_numclks-2;
        inq := i_in;
      else
        if cnt > 0 then
          cnt := cnt - 1;
        else
          o_out <= i_in;
        end if;
      end if;
    end if;
  end process;

end;


-------------------------------------------------------------------------------
-- vlm5030_delay_inv
--
-- Invert input signal and delay falling edge of input.
-- The input's rising edge is not delayed.
--
-- NOTE: This is inertial delay.
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.clock_functions_pack.all;

entity vlm5030_delay_inv is

  generic (
    g_numclks : integer := 3
  );
  port (
    i_clk : in  r_clk;
    i_in  : in  std_logic;
    o_out : out std_logic
  );

end;

architecture rtl of vlm5030_delay_inv is
  signal outq : std_logic;
begin

  delay_p : process (i_clk)
    variable cnt : natural := 0;
    variable inq : std_logic := '0';
  begin
    if rising_edge(i_clk) then
      if i_in /= inq then
        cnt := g_numclks-2;
        inq := i_in;
        if i_in = '1' then
          outq <= '1';
        end if;
      else
        if cnt > 0 then
          cnt := cnt - 1;
        else
          outq <= i_in;
        end if;
      end if;
    end if;
  end process;

  o_out <= '0' when i_in = '1' else not outq;

end;
