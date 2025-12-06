library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity srom02 is
  port (
    clk_i  : std_logic;
    addr_i : std_logic_vector(3 downto 0);
    data_o : out std_logic_vector(7 downto 0));
end srom02;

architecture behav of srom02 is
begin
  process (clk_i, addr_i)
    type mem_type is array (16 downto 1) of std_logic_vector (7 downto 0);
    constant mem : mem_type := (
      "XXXXXXXX", x"e1", x"d2", x"c3",
      x"b4", x"a5", x"96", x"87",
      x"78", x"69", x"5a", x"4b",
      x"3c", x"2d", x"1e", x"3f");
    variable addr : natural range mem_type'range;
  begin
    if rising_edge(clk_i) then
      addr := to_integer (unsigned (addr_i));
      data_o <= mem (addr);
    end if;
  end process;
end behav;
