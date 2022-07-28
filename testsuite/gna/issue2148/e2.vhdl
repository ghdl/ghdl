entity e2 is
  port (v : out natural);
end;

architecture behav of e2 is
begin
  process
  begin
    report (1 to v => 'X');
    wait;
  end process;
end;
