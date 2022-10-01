library ieee;
use ieee.std_logic_1164.all;

entity repro is
  generic (w : integer := 8);
  port (a, b : std_logic_vector (w - 1 downto 0);
        res : out std_logic_vector (w - 1 downto 0));
end repro;

architecture behav of repro is
begin
  process
  begin
    report "w = " & natural'image(w);
    if w <= 0 then
      assert false
        report "w value is <= 0"
        severity error;
    end if;
    if w > 128 then
      assert false
        report "w value is large"
        severity note;
    end if;
    wait;
  end process;

  res <= a or b;
end behav;
