entity PROBLEM is
end PROBLEM;

architecture BUG of PROBLEM is

  -- original testcase used std_logic_vector but other types suffer too
  type t_int_ptr is access integer; 

  function ISSUE_HERE return t_int_ptr is
  begin 
    return new integer; 
  end ISSUE_HERE;

  -- do functions with parameters work?
  function ISSUE_2(I : Integer) return t_int_ptr is
  variable Temp : t_int_ptr;
  begin 
    Temp := new integer; 
    Temp.all := I;
    return Temp;
  end ISSUE_2;

  function ISSUE_3 return t_int_ptr is
  variable Temp : t_int_ptr;
  begin 
    Temp := new integer; 
    Temp.all := 33;
    return Temp;
  end ISSUE_3;

  -- original testcase passed the result as param to a procedure
  -- so test passing parameters too
  procedure ANY_STUFF(param: in integer) is 
  begin
    report "Integer value " & integer'image(param) severity note;
  end procedure;

begin

  eval : process is
    variable X : t_int_ptr;
    variable Y : integer;
  begin
    X := ISSUE_HERE;
    ANY_STUFF(X.all);		-- Test case (1) : works
    --Y := ISSUE_2(55).all;  	-- Test case (2) : used to fail; works with first patch
    --ANY_STUFF(Y);
    Y := ISSUE_HERE.all; 	-- Test case (3) : fails
    ANY_STUFF(Y);
    ANY_STUFF(ISSUE_HERE.all);  -- Test case (4) : fails
    Y := ISSUE_3.all; 	-- Test case (5) : fails
    ANY_STUFF(Y);
    ANY_STUFF(ISSUE_3.all);  -- Test case (6) : fails
    wait;
  end process;   

end BUG;
