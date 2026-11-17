library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram01 is
  port (clk, we : in std_logic;
        wa, ra : in std_logic_vector(1 downto 0);
        d : in std_logic_vector(7 downto 0);
        q : out std_logic_vector(7 downto 0));
end entity;

architecture rtl of ram01 is
  type mem_t is array (0 to 3) of std_logic_vector(7 downto 0);
  signal mem : mem_t;
begin
  process (clk)
    variable v : mem_t;
  begin
    if rising_edge(clk) then
      v := mem;
      if we = '1' then
        v(to_integer(unsigned(wa))) := d;
      end if;
      mem <= v;
      --  Write-first: reads D when WA = RA.
      q <= v(to_integer(unsigned(ra)));
    end if;
  end process;
end architecture;
