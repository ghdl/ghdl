entity repro is
  generic (v : time := 12 ns);
end;

architecture behav of repro is
begin
  process
  begin
    report '<' & time'image(v) & '>';
    wait;
  end process;
end;
