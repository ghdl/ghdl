library ieee;
use ieee.std_logic_1164.all;

entity bug_tb is

end bug_tb;

-------------------------------------------------------------------------------

architecture test of bug_tb is

  type t_test_vec is array (10 downto -1) of std_logic;
  signal test_vec : t_test_vec := (others => '0');

  -- clock
  signal Clk : std_logic := '1';

  procedure pr_vec (
    vec : in std_logic_vector) is
  begin  -- procedure pr_vec
    for i in vec'range loop
      report "bit: " & integer'image(i) & "=" & std_logic'image(vec(i)) severity note;
    end loop;  -- i
  end procedure pr_vec;
  
begin  -- test

  -- clock generation
  Clk <= not Clk after 10 ns;

  -- waveform generation
  WaveGen_Proc : process
  begin
    wait until rising_edge(Clk);
    pr_vec(std_logic_vector(test_vec));
    wait;
  end process WaveGen_Proc;

end test;

