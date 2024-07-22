library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi_slave is
  port (
    spi_clk : in std_logic;
    spi_out : out std_logic;
    spi_en : in std_logic
    );
end spi_slave;

architecture rtl of spi_slave is
begin
  process(spi_clk)
    variable bit_cnt : unsigned(3 downto 0);
  begin
    if spi_en = '0' then	
        if rising_edge(spi_clk) then
            bit_cnt := bit_cnt + 1;
        end if;

        if falling_edge(spi_clk) then
            if bit_cnt (1) = '1' then
              spi_out <= spi_en;
            end if;
        end if;
    else
      bit_cnt := x"0";
      spi_out <= '0';
    end if;
  end process;
end rtl;
