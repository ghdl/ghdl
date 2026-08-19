library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity tb is
end tb;

architecture Behavioral of tb is

constant c_width      : integer := 300;
constant c_heigth     : integer := 280;

signal test : std_logic_Vector(7 downto 0);
subtype t_data is std_logic_vector(7 downto 0);

-- type of two dimensional array of data type
type t_two_dim_array is array(0 to c_heigth-1, 0 to c_width-1) of t_data;

signal i : integer := 0;
signal j : integer := 0;

signal image : t_two_dim_array := (others => (others => (others => '0')));
signal nclk	:	std_logic;
begin

clk_process :process
begin
  nclk <= '0';
  wait for 5 ns;
  nclk <= '1';
  wait for 5 ns;
end process;


process(nclk)
begin
  if (rising_edge(nclk)) then
	test <= image(i,j);
	j <= j + 1;
	if (j = c_width -1) then
		j <= 0;
		i <= i + 1;
	end if;
	if (i = c_heigth -1) then
		i <= 0;
	end if;
  end if;
end process;

process
begin
  wait for 100 ns;
  std.env.finish;
end process;

end Behavioral;
