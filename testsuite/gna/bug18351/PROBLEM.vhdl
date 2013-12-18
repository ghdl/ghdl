library ieee;
use ieee.std_logic_1164.all;


entity PROBLEM is
end PROBLEM;


architecture BUG of PROBLEM is
  type t_stdlogic_ptr is access std_logic_vector;
  function ISSUE_HERE return t_stdlogic_ptr;

  procedure PROBLEM_INSIDE is
    procedure ANY_STUFF(param: in std_logic_vector) is
    begin
    end procedure;
  begin
    ANY_STUFF(ISSUE_HERE.all);
  end PROBLEM_INSIDE;

begin

  

end BUG;
