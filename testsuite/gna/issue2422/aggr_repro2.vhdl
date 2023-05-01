library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity comp_repro2 is
	port (
		output : out unsigned
	);
end entity;

architecture a1 of comp_repro2 is
begin
	output <= (others => '0');  -- not using others due to issue #2421
end architecture;


entity aggr_repro2 is
end;

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

architecture behav of aggr_repro2 is
  signal s : unsigned(7 downto 0);
begin
  inst: entity work.comp_repro2
    port map (output => s);

  process
  begin
    wait for 1 ns;
    assert s = (s'range => '0') severity failure;
    wait;
  end process;
end behav;
