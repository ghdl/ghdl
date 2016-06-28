entity test is
end;

use work.pkg;
use work.mygpkg;
architecture behav of test is
begin
   assert mygpkg.test = 17;
end behav;
