entity err_flt01 is
  port (v : real;
        o : out boolean);
end;

architecture rtl of err_flt01 is
begin
  o <= v < 1.2;
end;
