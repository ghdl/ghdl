package support_pkg3 is
  function ">" ( l,r : integer ) return bit;
end;

package body support_pkg3 is
  function ">" ( l,r : integer ) return bit is
  begin
    if boolean'(l > r) then
      return '1';
    else
      return '0';
    end if;
  end ">";
end;

use work.support_pkg3.all;

entity repro3 is
end;

architecture behav of repro3 is
  signal sov : bit_vector(0 to 21);
begin
  assert (sov'length > 1);
end;
