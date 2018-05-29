--! @author Trip Richert

library ieee;
use ieee.std_logic_1164.all;

package LinkedListPkg is
  generic (
    type elem_type
    );

  type LinkedList;
  type LinkedListPtr is access LinkedList;
  
  type LinkedList is record
    elem : elem_type;
    nextPtr : LinkedListPtr;
  end record LinkedList;

  
end package LinkedListPkg;

