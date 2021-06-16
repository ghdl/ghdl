library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity imem2a is
  port (
    clk_i  : in  std_ulogic;
    addr_i : in  std_ulogic_vector(30 downto 0);
    data_i : in  std_ulogic_vector(15 downto 0);
    data_o : out std_ulogic_vector(15 downto 0)
  );
end entity;

architecture notok of imem2a is
  type ram_t is array(0 to 2**8-1) of std_ulogic_vector(15 downto 0);
begin
  process(clk_i)
    variable memory : ram_t;
  begin
    if rising_edge(clk_i) then
      memory(to_integer(unsigned(addr_i))) := data_i;
      data_o <= memory(to_integer(unsigned(addr_i)));
    end if;
  end process;
end architecture;
