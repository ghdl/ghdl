entity bar is
end entity bar;
entity \foo\ is
  port (test : in bit);
end entity \foo\;
architecture structural of \foo\ is
begin  -- architecture structural
end architecture structural;
architecture structural of bar is
  signal test : bit;
begin  -- architecture structural
  foo_1: entity work.\foo\
    port map (test => test);
end architecture structural;
