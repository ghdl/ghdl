package ppkg1 is
  type line is access string;
  procedure rep1 (variable msg : line := new string (1 to 7));
  procedure rep2;
  procedure rep3;
end ppkg1;

package body ppkg is
  procedure rep1 (msg : line := new string (1 to 7)) is
  begin
    msg.all := (others => ' ');
  end rep1;
  
  procedure rep2 is
  begin
    rep1;
    rep1;
  end rep2;
  
  procedure rep3 is
  begin
    rep1;
  end rep3;
end ppkg;
