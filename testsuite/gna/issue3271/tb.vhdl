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

--

library ieee;
use ieee.std_logic_1164.all;

use work.pkg.all;

entity tb is
begin end;

architecture behav of tb is
  signal source : st_t;
  signal valid  : std_logic;
  signal ready  : std_logic;
begin
  inst : entity work.ent
    port map (
      source => source,
      valid  => valid,
      ready  => ready
    );

  proc : process is
  begin
    valid <= '0';
    wait for 1 ns;
    assert source.valid = '0';

    valid <= '1';
    wait for 1 ns;
    assert source.valid = '1';

    source.ready <= '0';
    wait for 1 ns;
    assert ready = '0';

    source.ready <= '1';
    wait for 1 ns;
    assert ready = '1';

    wait;
  end process;
end architecture;
