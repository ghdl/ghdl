library ieee;
use ieee.std_logic_1164.all;

entity rom_constant is
  port (
    clk : in std_logic;
    a : out std_logic_vector(7 downto 0)
  );
end rom_constant;

architecture rtl of rom_constant is
  constant C_IEND : std_logic_vector(12*8-1 downto 0) := (others => '1');
  signal index : integer := 0;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      a <= C_IEND(index*8-1 downto (index-1)*8);

      if index < 12 then
        index <= index + 1;
      else
        index <= 0;
      end if;
    end if;
  end process;
end rtl;

