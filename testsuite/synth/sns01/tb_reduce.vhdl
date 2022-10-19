library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;

entity tb_reduce is
end;

architecture behav of tb_reduce is
  signal vec       : std_logic_vector (7 downto 0);
  signal res_and   : std_logic;
  signal res_nand  : std_logic;
  signal res_or    : std_logic;
  signal res_nor   : std_logic;
  signal res_xor   : std_logic;
  signal res_xnor  : std_logic;
begin
  dut: entity work.reduce
    port map (vec => vec,
              res_and  => res_and,
              res_nand => res_nand,
              res_or   => res_or,
              res_nor  => res_nor,
              res_xor  => res_xor,
              res_xnor => res_xnor);

  process
  begin
    vec <= "00000000";
    wait for 1 ns;
    assert res_and  = '0' severity failure;
    assert res_nand = '1' severity failure;
    assert res_or   = '0' severity failure;
    assert res_nor  = '1' severity failure;
    assert res_xor  = '0' severity failure;
    assert res_xnor = '1' severity failure;

    vec <= "00010000";
    wait for 1 ns;
    assert res_and  = '0' severity failure;
    assert res_nand = '1' severity failure;
    assert res_or   = '1' severity failure;
    assert res_nor  = '0' severity failure;
    assert res_xor  = '1' severity failure;
    assert res_xnor = '0' severity failure;

    vec <= "00100010";
    wait for 1 ns;
    assert res_and  = '0' severity failure;
    assert res_nand = '1' severity failure;
    assert res_or   = '1' severity failure;
    assert res_nor  = '0' severity failure;
    assert res_xor  = '0' severity failure;
    assert res_xnor = '1' severity failure;

    vec <= "11111111";
    wait for 1 ns;
    assert res_and  = '1' severity failure;
    assert res_nand = '0' severity failure;
    assert res_or   = '1' severity failure;
    assert res_nor  = '0' severity failure;
    assert res_xor  = '0' severity failure;
    assert res_xnor = '1' severity failure;

    wait;
  end process;
end behav;
