entity tb_case02 is
end tb_case02;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_case02 is
  signal s : std_logic_vector (4 downto 0);
  signal o : std_logic;
begin
  dut: entity work.case02
    port map (s, o);

  process
    constant ov : std_logic_vector (0 to 31) :=
      b"00010011000010001100000000000000";
  begin
    for i in ov'range loop
      s <= std_logic_vector(to_unsigned(i, 5));
      wait for 1 ns;
      assert o = ov(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
