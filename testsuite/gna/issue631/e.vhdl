entity e is end entity;
architecture a of e is begin
  process
    procedure p(x :out bit_vector(1 to 1)) is begin end procedure;
    function f(x :bit_vector) return string is begin return "a"; end function;
    variable v :string(1 to 1);
  begin
    p( f(x)(1) => v(1) );
    wait;
  end process;
end architecture;
