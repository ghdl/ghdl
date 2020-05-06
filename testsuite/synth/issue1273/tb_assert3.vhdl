entity tb_assert3 is
  generic (with_err : boolean := False);
end tb_assert3;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_assert3 is
  signal v : std_logic_Vector (7 downto 0);
  signal en : std_logic := '0';
  signal res : natural;
begin
  dut: entity work.assert3
    port map (v, en, res);

  process
  begin
    wait for 1 ns;

    en <= '1';
    v <= b"0010_0000";
    wait for 1 ns;
    assert res = 5 severity failure;

    --  Trigger an error.
    if with_err then
      en <= '1';
      v <= b"0000_0010";
      wait for 1 ns;
      assert res = 1 severity failure;
    end if;

    wait;
  end process;
end behav;
