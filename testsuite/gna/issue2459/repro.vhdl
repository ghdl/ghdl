library ieee;
use ieee.std_logic_1164.all;

entity repro is
end entity;

architecture a of repro is
  procedure p (signal sig : out std_logic_vector; var : std_logic_vector) is
  begin
    sig <= (var'reverse_range => '0');
  end p;

  signal s : std_logic_vector(1 downto 0);
begin
  process
    variable v : std_logic_vector(1 downto 0);
  begin
    p (s, v);
    wait;
  end process;
end;
