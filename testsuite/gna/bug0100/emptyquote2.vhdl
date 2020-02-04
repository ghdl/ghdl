entity emptyquote is
end;

architecture behav of emptyquote is
  procedure proc is
  begin
    null;
  end proc;
begin
  process
  begin
    proc'  '
    null;
  end process;
end;
