entity tb_assert4 is
  generic (with_err : boolean := False);
end tb_assert4;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_assert4 is
  signal v : std_logic_Vector (7 downto 0);
  signal en : std_logic := '0';
  signal clk : std_logic;
  signal res : std_logic;
begin
  dut: entity work.assert4
    port map (v, en, clk, res);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    en <= '1';
    v <= b"0010_0000";
    pulse;
    assert res = '0' severity failure;

    v <= b"0010_0001";
    pulse;
    assert res = '1' severity failure;

    v <= b"0010_0011";
    pulse;
    assert res = '0' severity failure;

    v <= b"0010_0010";
    pulse;
    assert res = '1' severity failure;

    en <= '0';
    v <= x"00";
    pulse;
    assert res = '1' severity failure;
    
    --  Trigger an error.
    if with_err then
      en <= '1';
      pulse;
    end if;

    wait;
  end process;
end behav;
