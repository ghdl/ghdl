ENTITY repro IS
END repro;

package genpkg is
  generic (function match (l, R : integer) return boolean);
  procedure comp (l, R : integer; res : out boolean);
end genpkg;

package body genpkg is
  procedure comp (l, R : integer; res : out boolean) is
  begin
    res := match (l, r);
  end comp;
end genpkg;

package my_pkg is new work.genpkg generic map (match => "=");

use work.my_pkg.all;

ARCHITECTURE behav OF repro IS
BEGIN
   PROCESS
      variable ok : boolean;
   BEGIN
      comp (5, 2 + 3, ok);
      --ok := my_pkg.comp (5, 2 + 3);
      assert ok severity error;
      wait;
   END PROCESS;
end behav;
