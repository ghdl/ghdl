library ieee;
use ieee.std_logic_1164.all;

entity err02 is
  generic (p : natural := 7);
  port (a, b, c : in std_logic;
        z : out std_logic);
end;

architecture behav of err02 is
  constant s : string (1 to 6) := "Hello!";

  function f_r return boolean is
  begin
    --  Error on condition
    if s(p) /= ' ' then
      return false;
    end if;
  end;

  constant res : boolean := f_r;
begin

  process(A, B, C)
    variable temp : std_logic;
  begin
    temp := A and B;
    Z <= temp or C;
  end process;
end behav;
