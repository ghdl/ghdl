entity tb_fsm_2s is
end tb_fsm_2s;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_fsm_2s is
  signal clk : std_logic;
  signal rst : std_logic;
  signal din : std_logic;
  signal done : std_logic;
begin
  dut: entity work.fsm_2s
    port map (
      done => done,
      d => din,
      clk => clk,
      rst => rst);

  process
    constant dat : std_logic_vector := b"10010";
    constant res : std_logic_vector := b"01001";
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rst <= '1';
    din <= '0';
    pulse;
    assert done = '0' severity failure;
    --  Test the whole sequence.
    rst <= '0';
    for i in dat'range loop
      din <= dat (i);
      pulse;
      assert done = res(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
