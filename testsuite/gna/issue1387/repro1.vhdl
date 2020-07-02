package support_pkg is
  function ">" ( l,r : integer ) return bit;
end support_pkg;

use work.support_pkg.all;

entity repro1 is
end;

architecture behav of repro1 is
  signal sov : bit_vector(0 to 21);
begin
  cfg : if sov'length > 1 generate
  end generate;
end;
