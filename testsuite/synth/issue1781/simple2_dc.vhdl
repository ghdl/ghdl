library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity simple2_dc is
  port (
    rclk_i : in  std_ulogic;
    wclk_i : in  std_ulogic;
    rden_i : in  std_ulogic;
    wren_i : in  std_ulogic;
    addr_i : in  std_ulogic_vector(7 downto 0);
    data_i : in  std_ulogic_vector(15 downto 0);
    data_o : out std_ulogic_vector(15 downto 0)
  );
end entity;

architecture notok of simple2_dc is
  type ram_t is array(0 to 2**8-1) of std_ulogic_vector(15 downto 0);
begin
  process(wclk_i, rclk_i)
    variable memory : ram_t;
  begin
    if rising_edge(wclk_i) then
      if wren_i = '1' then
        memory(to_integer(unsigned(addr_i))) := data_i;
      end if;
    end if;
    if rising_edge(rclk_i) then
      if rden_i = '1' then
        data_o <= memory(to_integer(unsigned(addr_i)));
      end if;
    end if;
  end process;
end architecture;
