entity tb_implicit_wide4 is
end tb_implicit_wide4;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_implicit_wide4 is
  signal clk  : std_logic;
  signal gate : std_logic;
  signal d1   : std_logic_vector(1 downto 0);
  signal q1   : std_logic_vector(1 downto 0);
  signal q2   : std_logic_vector(1 downto 0);
  signal q3   : std_logic_vector(1 downto 0);
  signal d2   : unsigned(1 downto 0);
  signal q4   : unsigned(1 downto 0);
  signal q5   : unsigned(1 downto 0);
  signal q6   : unsigned(1 downto 0);
  signal d3   : signed(1 downto 0);
  signal q7   : signed(1 downto 0);
  signal q8   : signed(1 downto 0);
  signal q9   : signed(1 downto 0);
begin
  dut: entity work.implicit_wide4
    port map (
      clk  => clk,
      gate => gate,
      d1   => d1,
      q1   => q1,
      q2   => q2,
      q3   => q3,
      d2   => d2,
      q4   => q4,
      q5   => q5,
      q6   => q6,
      d3   => d3,
      q7   => q7,
      q8   => q8,
      q9   => q9);
  
  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    d1 <= "00";
    d2 <= "00";
    d3 <= "00";

    gate <= '0';
    pulse;
    assert q1 = "11" severity failure;
    assert q2 = "11" severity failure;
    assert q3 = "11" severity failure;
    assert q4 = "11" severity failure;
    assert q5 = "11" severity failure;
    assert q6 = "11" severity failure;
    assert q7 = "11" severity failure;
    assert q8 = "11" severity failure;
    assert q9 = "11" severity failure;

    gate <= '1';
    pulse;
    assert q1 = "11" severity failure;
    assert q2 = "00" severity failure;
    assert q3 = "00" severity failure;
    assert q4 = "11" severity failure;
    assert q5 = "00" severity failure;
    assert q6 = "00" severity failure;
    assert q7 = "11" severity failure;
    assert q8 = "00" severity failure;
    assert q9 = "00" severity failure;

    d1 <= "01";
    d2 <= "01";
    d3 <= "01";

    gate <= '0';
    pulse;
    assert q1 = "11" severity failure;
    assert q2 = "10" severity failure;
    assert q3 = "10" severity failure;
    assert q4 = "11" severity failure;
    assert q5 = "10" severity failure;
    assert q6 = "10" severity failure;
    assert q7 = "11" severity failure;
    assert q8 = "10" severity failure;
    assert q9 = "10" severity failure;

    gate <= '1';
    pulse;
    assert q1 = "10" severity failure;
    assert q2 = "00" severity failure;
    assert q3 = "01" severity failure;
    assert q4 = "10" severity failure;
    assert q5 = "00" severity failure;
    assert q6 = "01" severity failure;
    assert q7 = "10" severity failure;
    assert q8 = "00" severity failure;
    assert q9 = "01" severity failure;

    d1 <= "10";
    d2 <= "10";
    d3 <= "10";

    gate <= '0';
    pulse;
    assert q1 = "11" severity failure;
    assert q2 = "01" severity failure;
    assert q3 = "01" severity failure;
    assert q4 = "11" severity failure;
    assert q5 = "01" severity failure;
    assert q6 = "01" severity failure;
    assert q7 = "11" severity failure;
    assert q8 = "01" severity failure;
    assert q9 = "01" severity failure;

    gate <= '1';
    pulse;
    assert q1 = "01" severity failure;
    assert q2 = "00" severity failure;
    assert q3 = "10" severity failure;
    assert q4 = "01" severity failure;
    assert q5 = "00" severity failure;
    assert q6 = "10" severity failure;
    assert q7 = "01" severity failure;
    assert q8 = "00" severity failure;
    assert q9 = "10" severity failure;

    wait;
  end process;
end behav;
