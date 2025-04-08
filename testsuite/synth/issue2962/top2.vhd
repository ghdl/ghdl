library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top2 is
	port (
		sel   : in  std_logic_vector(2 downto 0);
		idata : in  std_logic_vector(15 downto 0);
		odata : out std_logic_vector(1 downto 0)
	);
end;

architecture synth of top2 is
  signal seln : natural;
begin
  seln <= to_integer(unsigned(sel)) * 2;
  odata <= idata(seln +1 downto seln);

end architecture;
