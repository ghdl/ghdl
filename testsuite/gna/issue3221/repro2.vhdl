library ieee;
context ieee.ieee_std_context;
use std.env.finish;

entity ent is end entity;

architecture arch of ent is

  procedure increment2(signal clk : in std_logic; signal count : inout integer) is
  begin
    report "procedure started";
    wait until rising_edge(clk);
    count <= count + 1;
    report "procedure finished";
  end procedure;

  signal clk : std_logic := '0';
  signal count_signal : integer := 0;

begin

  clk_p : process
  begin
    wait for 10 ns;
    clk <= not clk;
  end process;

  tb_p : process
  begin
    report "original count_signal: " & to_string(count_signal);
    increment2(clk, count_signal);
    wait until rising_edge(clk);
    wait until rising_edge(clk);
    report "incremented count_signal: " & to_string(count_signal);
    finish;
  end process;

end architecture;
