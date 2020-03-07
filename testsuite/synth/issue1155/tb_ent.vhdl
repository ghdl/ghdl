library ieee;
use ieee.std_logic_1164.all;

entity tb_ent is
end;

architecture a of tb_ent is
	component ent is
		port (
			clk   : in std_logic;
			write : in std_logic;

			addr       : in std_logic_vector(1 downto 0);
			data_write : in std_logic_vector(3 downto 0);

			x0 : out std_logic_vector(3 downto 0);
			x1 : out std_logic_vector(3 downto 0);
			x2 : out std_logic_vector(3 downto 0);
			x3 : out std_logic_vector(3 downto 0)
		);
	end component;

	signal clk, write : std_logic;
	signal addr       : std_logic_vector(1 downto 0);
	signal data_write : std_logic_vector(3 downto 0);
	signal x0, x1, x2, x3 : std_logic_vector(3 downto 0);
begin
	uut_inst: ent
		port map (
			clk   => clk,
			write => write,

			addr => addr,
			data_write => data_write,

			x0 => x0,
			x1 => x1,
			x2 => x2,
			x3 => x3
		);

	process
		procedure pulse is
		begin
			clk <= '0';
			wait for 10 ns;
			clk <= '1';
			wait for 10 ns;
		end;
	begin
		write <= '0';
		addr <= "00";

		pulse;

		write <= '1';
		data_write <= "1111";

		pulse;

		assert x0 = "1111";

		write <= '0';
		data_write <= "0001";

		pulse;

		assert x0 = "1111";

		wait for 20 ns;

		wait;
	end process;
end;
