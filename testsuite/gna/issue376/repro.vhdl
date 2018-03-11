package pkg is
  type enum is ('a', 'b', 'c');
  type enum_vec is array (natural range <>) of enum;
  function resolved (v : enum_vec) return enum;
  subtype res_enum is (resolved) enum_vec;
end pkg;

package body pkg is
  function resolved (v : enum_vec) return enum is
  begin
    return v (v'left);
  end resolved;
end;

use work.pkg.all;

entity repro is
  port (p : inout res_enum (3 downto 0));
end repro;

architecture behav of repro is
  alias p_0 is p(0);
begin
  p_0 <= 'a' after 1 ns, 'b' after 4 ns;
end behav;
