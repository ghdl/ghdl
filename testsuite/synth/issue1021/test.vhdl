library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity test is
    generic(
        ROW_BITS : integer := 4;
        WIDTH    : integer := 64
        );

    port(
        clk     : in  std_logic;
        rd_addr : in  std_logic_vector(ROW_BITS - 1 downto 0);
        rd_data : out std_logic_vector(WIDTH - 1 downto 0);
        wr_en   : in  std_logic;
        wr_sel  : in  std_logic_vector(WIDTH/8 - 1 downto 0);
        wr_addr : in  std_logic_vector(ROW_BITS - 1 downto 0);
        wr_data : in  std_logic_vector(WIDTH - 1 downto 0)
        );

end test;

architecture rtl of test is
    constant SIZE : integer := 2**ROW_BITS;

    type ram_type is array (0 to SIZE - 1) of std_logic_vector(WIDTH - 1 downto 0);
    signal ram : ram_type;

begin
    process(clk)
        variable lbit : integer range 0 to WIDTH - 1;
        variable mbit : integer range 0 to WIDTH - 1;
        variable widx : integer range 0 to SIZE - 1;
    begin
        if rising_edge(clk) then
            if wr_en = '1' then
                for i in 0 to WIDTH/8-1 loop
                    lbit := i * 8;
                    mbit := lbit + 7;
                    widx := to_integer(unsigned(wr_addr));
                    if wr_sel(i) = '1' then
                        ram(widx)(mbit downto lbit) <= wr_data(mbit downto lbit);
                    end if;
                end loop;
            end if;

            rd_data <= ram(to_integer(unsigned(rd_addr)));
        end if;
    end process;
end;
