package repro2_pkg1 is
  constant a : natural := 5;
end;

package repro2_pkg2 is
  constant a : natural := 6;
  constant b : natural := 1;
end;

use work.repro2_pkg1.all;
use work.repro2_pkg2.all;

package repro2_pkg3 is
  alias a is work.repro2_pkg2.b;
end;


