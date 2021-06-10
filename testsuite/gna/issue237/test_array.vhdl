library ieee ;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_array is
end;

architecture RTL of test_array is
  type mem_t is array (natural range <>) of std_logic_vector(31 downto 0);
  signal mem_down : mem_t(7 downto 0);
  signal num : natural := 0;
  signal clk : std_logic;
begin
  process
  begin
    for i in 1 to 4 loop
      num <= i;
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end loop;
    wait;
  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      for i in mem_down'range loop
        mem_down (i) <= std_logic_vector (to_unsigned (i, 32));
      end loop;
    end if;
  end process;
end RTL;
