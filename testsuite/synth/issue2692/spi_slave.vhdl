library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi_slave is
  port (
    spi_clk : in std_logic;
    spi_in : in std_logic;
    spi_out : out std_logic;
    spi_en : in std_logic;
    reg0_out : out std_logic_vector(7 downto 0);
    reg0_in : in std_logic_vector(7 downto 0)
    );
end spi_slave;

architecture rtl of spi_slave is
  signal reg_addr   : std_logic_vector(7 downto 0);
  signal wr         : std_logic;
  signal reg0_data : std_logic_vector(7 downto 0) := "00000000"; 
begin
  process(spi_clk)
    variable bit_cnt : integer;
  begin
    if spi_en = '0' then	
        wr <= reg_addr(7);

        if rising_edge(spi_clk) then
            if bit_cnt < reg_addr'length then
                reg_addr((reg_addr'length -1) - bit_cnt) <= spi_in; 
                spi_out <= '0';
            else
                if wr = '1' then
                    reg0_data(15 - bit_cnt) <= spi_in;
                end if;
            end if;

            bit_cnt := bit_cnt + 1;
        end if;

        if falling_edge(spi_clk) then
            if bit_cnt > reg_addr'length -1 then
                if wr = '0' then
                    spi_out <= reg0_in(15 - bit_cnt);
                end if;
            end if;
        end if;
    else
      bit_cnt := 0;
      spi_out <= '0';
      reg_addr <= "00000000";
      wr <= '0';
    end if;
  end process;

  reg0_out <= reg0_data;

end rtl;
