entity gen_delay is
  generic (function id(a : bit) return bit);

  port (i : in bit;
        o : out bit);
end gen_delay;

architecture arch of gen_delay is
begin
  o <= id (i);
end arch;

entity dly5 is
  port (i : in bit;
        o : out bit);
end dly5;

architecture struct of dly5 is
  component gen_delay is
    generic (function id(a : bit) return bit);
    port (i : in bit; o : out bit);
  end component;

  function inv(a : bit) return bit is
  begin
    return not a;
  end;
begin
  inst: gen_delay
    generic map (id => inv)
    port map (i => i, o => o);
end struct;
