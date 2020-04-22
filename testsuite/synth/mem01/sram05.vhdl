library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sram05 is
  port (
    rst : std_logic;
    clk_i  : std_logic;
    addr_i : std_logic_vector(3 downto 0);
    data_i : std_logic_vector(7 downto 0);
    data_o : out std_logic_vector(7 downto 0);
    wen_i   : std_logic);
end sram05;

architecture behav of sram05 is
begin
  process (clk_i, addr_i)
    type mem_type is array (0 to 15) of std_logic_vector (7 downto 0);
    variable mem : mem_type;
    variable addr : natural range mem_type'range;
  begin
    if rising_edge(clk_i) then
      if rst = '1' then
        null;
      else
        addr := to_integer (unsigned (addr_i));
        if wen_i = '1' then
          mem (addr) := data_i;
        end if;
        data_o <= mem (addr);
      end if;
    end if;
  end process;
end behav;
