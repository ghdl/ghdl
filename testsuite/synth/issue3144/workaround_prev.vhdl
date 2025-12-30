library ieee;
use ieee.std_logic_1164.all;

entity generic_type_prev is
  generic(type T);
  port(clk_i : in std_logic; data : T);
end entity;

architecture formal of generic_type_prev is 
	signal sl_data : std_logic;

	default clock is rising_edge(clk_i);

	signal data_reg : T;

begin

	-- workaround prev() by creating the the previous value manually (tmeissner trick)
	process(clk_i) begin
	if rising_edge(clk_i) then
		data_reg <= data;
	end if;
	end process;

	-- assert prev(sl_data) = sl_data; -- works, no generic type
	-- assert prev(data) = data; -- does not work, comment this line to prove that it works
        assert data_reg = data; -- workaround, does not work

end formal;
