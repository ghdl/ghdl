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


library ieee;
use ieee.std_logic_1164.all;

package vlm5030_pack is

  -----------------------------------------------------------------------------
  -- Verctorized NOR and OR functions
  -----------------------------------------------------------------------------
  function  norf(i : std_logic_vector) return std_logic;
  function  norf(wl, vec : std_logic_vector) return std_logic;
  function norif(wl, vec : std_logic_vector) return std_logic;
  function   orf(wl, vec : std_logic_vector) return std_logic;

end;

package body vlm5030_pack is

  function norf(i : std_logic_vector) return std_logic is
    variable lorf : std_logic;
  begin
    lorf := '0';
    for idx in i'range loop
      lorf := lorf or i(idx);
    end loop;
    return not lorf;
  end;

  function norf(wl, vec : std_logic_vector) return std_logic is
    variable lorf : std_logic;
  begin
    lorf := '0';
    for idx in wl'range loop
      lorf := lorf or (wl(idx) and vec(idx));
    end loop;
    return not lorf;
  end;

  function norif(wl, vec : std_logic_vector) return std_logic is
    variable lorf : std_logic;
  begin
    lorf := '0';
    for idx in wl'range loop
      lorf := lorf or (wl(idx) and not vec(idx));
    end loop;
    return not lorf;
  end;

  function orf(wl, vec : std_logic_vector) return std_logic is
  begin
    return not norf(wl, vec);
  end;

end;
