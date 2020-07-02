package support_pkg2 is
  function ">" ( l,r : integer ) return bit;
end;

use work.support_pkg2.all;

entity repro2 is
end;

architecture behav of repro2 is
  signal sov : bit_vector(0 to 21);
begin
  cfg : if ">"(sov'length, 1) generate
  end generate;
end;
