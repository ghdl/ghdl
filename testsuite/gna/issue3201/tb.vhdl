library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;


entity test is
	port (
		x : in std_logic_vector(3 downto 0)
	);
end entity;

architecture test_arch of test is
begin
end architecture;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity tb is
end entity;

architecture tb_arch of tb is
	signal testxy, testab : std_logic;
begin
	test_inst: entity work.test port map (
		x(3 downto 2) => testab & '0',
		x(1 downto 0) => testxy & '0'
	);
end architecture;
