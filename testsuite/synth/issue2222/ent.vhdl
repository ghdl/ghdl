library ieee;
use ieee.std_logic_1164.all;

entity ent is
  port (o : out std_ulogic);
end;

architecture a of ent is
  procedure proc (
    signal pin : out std_ulogic;
    constant drive_pin : boolean := false
    ) is
  begin
    if drive_pin then
      pin <= '1';
    end if;
  end procedure;
begin
  o <= '1';
  proc(pin => o);
end;
