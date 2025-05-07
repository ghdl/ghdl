library ieee;
use ieee.std_logic_1164.all;

entity tb_pb1 is
end;

architecture arch of tb_pb1 is
  signal rdb64  : std_logic_vector(63 downto 0);
  signal mask   : std_logic_vector(1 to 3);
  signal vaddr  : std_logic_vector(41 downto 12);
  signal result : std_logic_vector(63 downto 0);
begin
  DUT: entity work.pb1
    generic map (
      problem => 1)
    port map (
      rdb64  => rdb64,
      mask   => mask,
      vaddr  => vaddr,
      result => result);

  process
  begin
    rdb64 <= x"0000_0000_0000_0000";
    --         40       32       24       16
    vaddr <= b"11_11100000_11110000_11100011_0010";
    
    mask <= "011";
    wait for 1 ns;
    report "result(11 downto 3)=" & to_bstring(result(11 downto 3));
    assert result(11 downto 3) = b"000110010";

    wait;
  end process;
end arch;
