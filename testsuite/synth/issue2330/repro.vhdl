entity repro is
  port (a, b : bit_vector(7 downto 0);
        o : out bit);
end repro;

architecture arch of repro is
  function f (a, b : bit_vector(7 downto 0)) return boolean is
  begin
    return a = x"ca" and b = x"fe";
    end f;
begin
  o <= '1' when f(a,b) else '0';
end arch;
