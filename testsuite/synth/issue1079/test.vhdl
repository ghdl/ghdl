library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port(
	clk     : in  std_logic;

	rd_en   : in  std_logic;
	rd_addr : in  std_logic_vector(15 downto 0);
	rd_data : out std_logic_vector(63 downto 0);

	wr_en   : in  std_logic;
	wr_sel  : in  std_logic_vector(7 downto 0);
	wr_addr : in  std_logic_vector(15 downto 0);
	wr_data : in  std_logic_vector(63 downto 0)
	);
end test;

architecture rtl of test is
    constant SIZE : integer := 2**16;
    type ram_type is array (0 to SIZE - 1) of std_logic_vector(63 downto 0);
    signal ram : ram_type;
    signal rd_data0 : std_logic_vector(63 downto 0);
begin
    process(clk)
	variable widx : integer range 0 to SIZE - 1;
    begin
	if rising_edge(clk) then
	    if wr_en = '1' then
		widx := to_integer(unsigned(wr_addr));
                ram(widx) <= wr_data;
	    end if;
	    if rd_en = '1' then
		rd_data0 <= ram(to_integer(unsigned(rd_addr)));
	    end if;
	end if;
    end process;

    rd_data <= rd_data0;
end;
