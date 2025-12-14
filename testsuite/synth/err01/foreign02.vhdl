library ieee;
use ieee.std_logic_1164.all;

entity foreign02 is
  port (rst : std_logic;
        clk : in std_logic;
        counter : out natural);
end entity;

architecture a of foreign02 is
  function incr(signal i :  natural) return natural;
  attribute foreign of incr: function is "VHPIDIRECT incr";

  function incr(signal i :  natural) return natural is
  begin
    return i + 1;
  end incr;
  signal c : natural;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        c <= 0;
      else
        c <= incr(c);
      end if;
    end if;
  end process;

  counter <= c;
end;
