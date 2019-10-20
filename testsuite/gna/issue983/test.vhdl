library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;

architecture beh of test is

  signal s_clk      : std_logic;
  signal s_register : std_logic_vector(7 downto 0);

begin

  process is
  begin
    wait until rising_edge(s_clk);
    -- In VHDL code, hex/bin literals work
    s_register <= x"00";
    s_register <= b"00000000";
    s_register <= "00" & o"00";
  end process;

  default clock is rising_edge(s_clk);

  -- In PSL code they don't work
  FOOTER_VALID_hex : cover {s_register = x"00"};
  FOOTER_VALID_bin : cover {s_register = b"00000000"};
  FOOTER_VALID_oct : cover {s_register = "00" & o"00"};
  FOOTER_VALID : cover {s_register = "00000000"};

  process
  begin
    for i in 1 to 2 loop
      s_clk <= '0';
      wait for 1 ns;
      s_clk <= '1';
      wait for 1 ns;
    end loop;
    wait;
  end process;
end architecture beh;
