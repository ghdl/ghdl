library ieee ;
use ieee.std_logic_1164.all;

package mypackage is
  type myenum is (ONE, TWO, THREE);
  subtype myarray is bit_vector(2 downto 0);
  type myarray5 is array(1 downto 0) of bit;
end package;

library ieee ;
use ieee.std_logic_1164.all;
use work.mypackage.all;

entity myentity is
  generic (
    width: integer := 2;
    genenum: myenum := ONE;
    genarray1: bit_vector(1 downto 0) := "01";
    genarray3: myarray := "010";
    genarray5: myarray5 := ('1', '0')
    );
  port (
    portenum: in myenum;
    portarray1: in bit_vector(1 downto 0);
    portarray2: in bit_vector(width downto 0);
    portarray3: in myarray;
    portarray5: in myarray5
    );
end myentity;

architecture arch of myentity is
  subtype myarray4 is bit_vector(width downto 0);
  signal sigenum: myenum; 
  constant constenum: myenum := ONE;
  signal sigarray1: bit_vector(1 downto 0); 
  constant constarray1: bit_vector(1 downto 0) := "10";
  signal sigarray2: bit_vector(width downto 0); 
  constant constarray2: bit_vector(width downto 0) := (others => '1');
  signal sigarray3: myarray; 
  constant constarray3: myarray := "101";
  signal sigarray4: myarray4;
  constant constarray4: myarray4:= (others => '1');
  signal sigarray5: myarray5;
  constant constarray5: myarray5:= (others => '1');
begin
end arch; 
