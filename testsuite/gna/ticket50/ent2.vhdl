entity ent is
end entity;

package pkg1 is
  function cond return boolean;
end pkg1;

package pkg2 is
  function cond return boolean;
end pkg2;

use work.pkg1.all;
use work.pkg2.all;

architecture a of ent is
  type enum_t is (cond);
begin
  main : process
  begin
    if cond then
    end if;
  end process;
end architecture;
