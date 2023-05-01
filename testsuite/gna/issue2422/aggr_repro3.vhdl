library ieee;
use     ieee.std_logic_1164.all;

entity comp_repro3 is
	port (
		output : out std_logic_vector
	);
end entity;

architecture a1 of comp_repro3 is
begin
	output <= (7 downto 0 => '0');  -- not using others due to issue #2421
end architecture;


entity aggr_repro3 is
end;

library ieee;
use     ieee.std_logic_1164.all;

architecture behav of aggr_repro3 is
  signal s : std_logic_vector(7 downto 0);
begin
  inst: entity work.comp_repro3
    port map (output => s);

  process
  begin
    wait for 1 ns;
    assert s = (s'range => '0') severity failure;
    wait;
  end process;
end behav;
