entity tb_ent2 is
end tb_ent2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent2 is
  signal clk : std_logic;
  signal dout : std_logic_vector(3 downto 0);
  signal set_0 : std_logic;
  signal set_a : std_logic;
  signal set_f : std_logic;
  signal set_7 : std_logic;
begin
  dut: entity work.ent2
    port map (
      set_0 => set_0,
      set_a => set_a,
      set_f => set_f,
      set_7 => set_7,
      q => dout,
      clk => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    set_0 <= '1';
    set_a <= '0';
    set_f <= '0';
    set_7 <= '0';
    pulse;
    assert dout = x"0" severity failure;

    set_0 <= '0';
    set_a <= '0';
    set_f <= '0';
    set_7 <= '0';
    pulse;
    assert dout = x"1" severity failure;

    set_0 <= '0';
    set_a <= '0';
    set_f <= '0';
    set_7 <= '0';
    pulse;
    assert dout = x"2" severity failure;

    set_0 <= '0';
    set_a <= '1';
    set_f <= '0';
    set_7 <= '0';
    pulse;
    assert dout = x"a" severity failure;

    set_0 <= '0';
    set_a <= '0';
    set_f <= '0';
    set_7 <= '0';
    pulse;
    assert dout = x"b" severity failure;

    set_0 <= '0';
    set_a <= '0';
    set_f <= '1';
    set_7 <= '0';
    pulse;
    assert dout = x"f" severity failure;

    set_0 <= '0';
    set_a <= '0';
    set_f <= '0';
    set_7 <= '1';
    pulse;
    assert dout = x"7" severity failure;

    set_0 <= '1';
    set_a <= '0';
    set_f <= '0';
    set_7 <= '1';
    pulse;
    assert dout = x"0" severity failure;

    set_0 <= '0';
    set_a <= '1';
    set_f <= '0';
    set_7 <= '1';
    pulse;
    assert dout = x"a" severity failure;

    wait;
  end process;
end behav;
