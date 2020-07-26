library ieee;
use ieee.std_logic_1164.all;

entity repro is
  port (a : std_logic_vector (3 downto 0);
        o : out std_logic_vector (3 downto 0));
end;

architecture behav of repro is
  function cancel (a : std_logic_vector) return std_logic_vector
  is
    variable en : boolean := false;
    variable res : std_logic_vector (a'range);
  begin
    if a'length = 0 then
      en := true;
    end if;
    res := a;
    if en then
      res (7) := '0';
    end if;
    return res;
  end cancel;
begin
  o <= cancel (a);
end behav;

