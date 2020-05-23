library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram_blk is
    generic (
      AWIDTH : integer := 8;
      DWIDTH : integer := 64
      );

    port (
	clk     : in  std_logic;
	rd_addr : in  std_logic_vector(AWIDTH - 1 downto 0);
	rd_data : out std_logic_vector(DWIDTH - 1 downto 0);
	wr_en   : in  std_logic;
	wr_addr : in  std_logic_vector(AWIDTH - 1 downto 0);
	wr_data : in  std_logic_vector(DWIDTH - 1 downto 0)
	);

end ram_blk;

architecture rtl of ram_blk is
  type ram_type is
    array (0 to 2**AWIDTH - 1) of std_logic_vector(DWIDTH - 1 downto 0);

  signal ram : ram_type;
  attribute ram_style : string;
  attribute ram_style of ram : signal is "block";
  attribute ram_decomp : string;
  attribute ram_decomp of ram : signal is "power";
begin
    process(clk)
    begin
	if rising_edge(clk) then
	    if wr_en = '1' then
              ram(to_integer(unsigned(wr_addr))) <= wr_data;
	    end if;
            rd_data <= ram(to_integer(unsigned(rd_addr)));
	end if;
    end process;
end;
