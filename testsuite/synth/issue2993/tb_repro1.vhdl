library ieee;
use ieee.std_logic_1164.all;

entity tb_repro1 is
end;

architecture arch of tb_repro1 is
  signal mask   : std_logic_vector(1 to 3);
  signal vaddr  : std_logic_vector(41 downto 12);
  signal result : std_logic_vector(8 downto 0);
begin
  DUT: entity work.repro1
    port map (
      mask   => mask,
      vaddr  => vaddr,
      result => result);

  process
  begin
    --         40       32       24       16
    vaddr <= b"11_11100000_11110000_11100011_0010";
    mask <= "000";
    wait for 1 ns;
    report "result=" & to_bstring(result);
    assert result =  b"0_00110010";

    mask <= "010";
    wait for 1 ns;
    report "result=" & to_bstring(result);
    assert result =  b"110000011";
wait;
  end process;
end arch;
