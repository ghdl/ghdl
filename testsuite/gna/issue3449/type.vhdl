package q1 is
  type T is (a, b);
end package;

package q2 is
  alias T is work.q1.T;
end package;

use work.q1.all, work.q2.all;
entity e is
end;

architecture arch of e is
begin
  process
  begin
    report T'path_name;  -- GHDL: :work:q1:t
    wait;
  end process;
end;
