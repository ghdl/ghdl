library ieee;
use ieee.std_logic_1164.all;

entity ent2 is
	port (
		clk : in std_logic;
		i : in std_logic_vector(31 downto 0);
		o : out std_logic_vector(7 downto 0)
	);
end;

architecture a of ent2 is
  subtype word is std_logic_vector(31 downto 0);

  function switch_endianness(x : word) return word is
  begin
    return x(7 downto 0) & x(15 downto 8) & x(23 downto 16) & x(31 downto 24);
  end function;
begin
	process(clk)
	begin
		if rising_edge(clk) then
			o <= switch_endianness(i)(7 downto 0);
		end if;
	end process;
end;
