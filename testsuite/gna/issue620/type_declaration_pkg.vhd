library ieee;
use ieee.std_logic_1164.all;

package type_declaration_pkg is
  generic (
    constant numElem : natural
    );
  type myType is array(numElem - 1 downto 0) of natural;
  function unity(val : myType) return myType;

end package type_declaration_pkg;

package body type_declaration_pkg is
  function unity(val : myType) return myType is
  begin
    return val;
  end function unity;

end package body type_declaration_pkg;
