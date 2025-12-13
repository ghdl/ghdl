library ieee;
use ieee.std_logic_1164.all;

entity foreign01 is
  port (rst : std_logic;
        clk : in std_logic;
        counter : out natural);
end entity;

architecture a of foreign01 is
  procedure incr(signal i : inout natural);
  attribute foreign of incr: procedure is "VHPIDIRECT incr";
  
  procedure incr(signal i : inout natural) is
  begin
    i <= i + 1;
  end procedure;
  signal c : natural;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        c <= 0;
      else
        incr(c);
      end if;
    end if;
  end process;

  counter <= c;
end;
