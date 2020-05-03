library ieee;
use ieee.std_logic_1164.all;

entity case03 is
	port (
		clk : in std_logic;
		opc : in std_logic_vector (2 downto 0);
		arg : in std_logic_vector (7 downto 0);
		res : out std_logic_vector (7 downto 0);
		par : out std_logic);
end case03;

architecture behav of case03 is
  signal result : std_logic_vector(7 downto 0);
  signal parity : std_logic;
begin
	process (clk)
	begin
		if rising_edge(clk) then
		  res <= result;
		  par <= parity;
		end if;
	end process;

	op : process (opc, arg)
		variable t : std_logic_vector(7 downto 0);
		variable p : std_logic;
	begin
	  p := '0';
	  case opc is
		when "000" | "001" | "010" | "011" =>
		  t := not arg;

		  for i in t'range loop
			p := p xor t(i);
		  end loop;
		when others =>
		  t := arg;
	  end case;
	  result <= t;
	  parity <= p;
	end process;
end behav;

