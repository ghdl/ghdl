package ppkg is
  procedure rep1 (msg : string := "failure");
  procedure rep2;
  procedure rep3;
end ppkg;

package body ppkg is
  procedure rep1 (msg : string := "failure") is
  begin
    report msg;
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
