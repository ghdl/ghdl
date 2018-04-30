library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
  port (
    key0: in std_logic;
    key3: in std_logic;
    counter_out: out std_logic_vector(3 downto 0)
  );
end counter;

architecture arch_counter of counter is
  signal c: std_logic_vector(0 to 3) := (others => '0');
begin
  process(key0, key3)
  begin
    if (rising_edge(key0)) then
      c <= std_logic_vector(unsigned(c) + 1);
      if (unsigned(c) = 9) then
        c <= "0000";
      end if;
    end if;
    if (rising_edge(key3)) then
      c <= std_logic_vector(unsigned(c) - 1);
      if (unsigned(c) = "1111") then
        c <= "1000";
      end if;
    end if;
  end process;

  counter_out <= c;
end arch_counter ; -- arch_counter
