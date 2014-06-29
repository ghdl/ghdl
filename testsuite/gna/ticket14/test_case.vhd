library ieee;
use ieee.std_logic_1164.all;
-- use ieee.numeric_std.all;

entity scrambler is
    generic (
	BUS_WIDTH : integer := 8;
	ARRAY_WIDTH : integer := 2);
    port (
	clk, en, reset, seed : in std_logic;
	d_in : in std_logic;
	d_out : out std_logic);
end entity scrambler;

architecture behavioural of scrambler is

    type test_array_type is array (ARRAY_WIDTH-1 downto 0) of
      std_logic_vector (BUS_WIDTH-1 downto 0);
    signal test_array : test_array_type := (others => (others => '0'));
    signal test_vec : std_logic_vector (BUS_WIDTH-1 downto 0) 
					:= (others => '0');

begin

    failing_process : process (clk) begin
	if clk'event and clk = '1' then
	    test_array <= test_array (ARRAY_WIDTH-2 downto 0) & test_vec;
	end if;
    end process failing_process;
    
end architecture behavioural;
