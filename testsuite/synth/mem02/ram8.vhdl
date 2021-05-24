library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram8 is
  port (val : out std_logic_vector (7 downto 0);
        addr : std_logic_vector (3 downto 0);
        clk : std_logic);
end ram8;

architecture behav of ram8 is
  type mem_t is array (15 downto 0) of std_logic_vector(7 downto 0);
  signal mem : mem_t;
begin
  process (clk)
    variable ra : natural;
  begin
    if rising_edge (clk) then
      ra := to_integer(unsigned (addr));
      val <= mem(ra);
    end if;
  end process;
end behav;
