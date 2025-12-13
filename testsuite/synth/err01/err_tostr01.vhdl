entity err_tostr01 is
  port (v : natural;
        o : out boolean);
end;

architecture behav of err_tostr01 is
begin
  o <= to_string(v) = "0";
end;
