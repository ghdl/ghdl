package p1 is
  constant K : integer := 1;
end package;

package p2 is
  alias K is work.p1.K;
end package;

use work.p1.all, work.p2.all;
entity e is end entity;
architecture rtl of e is
  signal s : integer := K;
begin
end architecture;
