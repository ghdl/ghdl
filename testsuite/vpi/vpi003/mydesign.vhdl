library ieee ;
use ieee.std_logic_1164.all;

package mypackage is
  type myarray2dim is array(1 downto 0, 1 downto 0) of bit;
  type myarray1 is array(1 downto 0) of bit_vector(1 downto 0);
end package;

library ieee ;
use ieee.std_logic_1164.all;
use work.mypackage.all;

entity myentity is
  generic (
    genarr2dim: in myarray2dim := (('0', '0'), ('1', '1'));
    genarray1: in myarray1 := ("10", "10")
    );
  port (
    portarr2dim: in myarray2dim;
    portarray1: in myarray1
    );
end myentity;

architecture arch of myentity is
  signal sigarray2dim: myarray2dim; 
  signal constarray2dim: myarray2dim := (('0', '0'), ('1', '1'));
  signal sigarray1: myarray1; 
  signal constarray1: myarray1 := ("10", "10");
begin
end arch; 
