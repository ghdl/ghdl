library ieee;
use     ieee.std_logic_1164.all;

package pkg_repro4 is
  subtype my_slv is std_ulogic_vector;
end;

library ieee;
use     ieee.std_logic_1164.all;
use work.pkg_repro4.all;

entity comp_repro4 is
	port (
		output : out my_slv
	);
end entity;

architecture a1 of comp_repro4 is
begin
	output <= (7 downto 0 => '0');  -- not using others due to issue #2421
end architecture;


entity aggr_repro4 is
end;

library ieee;
use     ieee.std_logic_1164.all;
use work.pkg_repro4.all;

architecture behav of aggr_repro4 is
  signal s : my_slv(7 downto 0);
begin
  inst: entity work.comp_repro4
    port map (output => s);

  process
  begin
    wait for 1 ns;
    assert s = (s'range => '0') severity failure;
    wait;
  end process;
end behav;
