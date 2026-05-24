library ieee;
use ieee.std_logic_1164.all;

package pkg is
  type st_t is record
    ready : std_logic;
    valid : std_logic;
  end record;

  view st_source_v of st_t is
    ready : in;
    valid : inout;
  end view;

  procedure st_pack(
    signal source : inout st_t;
    signal valid  : in std_logic;
    signal ready  : out std_logic
  );
end package;

package body pkg is
  procedure st_pack(
    signal source : inout st_t;
    signal valid  : in std_logic;
    signal ready  : out std_logic
  ) is
  begin
    source.valid <= valid;
    ready        <= source.ready;
    source.ready <= 'Z'; -- undriven
  end procedure;
end package body;

--

library ieee;
use ieee.std_logic_1164.all;

use work.pkg.all;

entity ent is
  port (
    source : view st_source_v of st_t;
    valid  : in std_logic;
    ready  : out std_logic
  );
begin end;

architecture behav of ent is
begin
  st_pack(source, valid, ready);
end architecture;
