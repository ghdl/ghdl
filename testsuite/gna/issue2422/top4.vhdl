library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity comp is
	port (
		output : out unsigned
	);
end entity;

architecture a1 of comp is
begin
end architecture;

library ieee;
use     ieee.std_logic_1164.all;

entity top is
	port(
		sig : out std_logic_vector
	);
end entity;

architecture a2 of top is
begin
	inst : entity work.comp
		port map (
			std_logic_vector(output) => sig
		);
end architecture;

library ieee;
use     ieee.std_logic_1164.all;

entity top4 is
end;

architecture behav of top4 is
  signal s : std_logic_vector(3 downto 0);
begin
  dut: entity work.top
    port map (sig => s);
end;
