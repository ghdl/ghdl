entity e1 is
  port (v : out natural);
end;

architecture behav of e1 is
begin
  process
  begin
    report natural'image(v);
    wait;
  end process;
end;
