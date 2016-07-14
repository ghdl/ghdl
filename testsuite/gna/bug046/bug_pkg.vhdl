package bug_pkg is
  procedure proc_bug(constant t : in  time := 8.68 us);
  procedure proc_ok1(constant t : in  time);
  procedure proc_ok2(constant t : in  integer := 5);
end bug_pkg;

package body bug_pkg is
  procedure proc_bug(constant t : in  time := 8.68 us) is
  begin
  end proc_bug;

  procedure proc_ok1(constant t : in  time) is
  begin
  end proc_ok1;

  procedure proc_ok2(constant t : in  integer := 5) is
  begin
  end proc_ok2;
end bug_pkg; 
