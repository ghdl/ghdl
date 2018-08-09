entity e is end entity;
architecture a of e is begin
  process
    procedure p(x :in bit_vector(1 to 1); y :out bit_vector(1 to 1)) is begin end procedure;
    function f(x :bit_vector) return string is begin return "aa"; end function;
    variable v :string(1 to 1);
  begin
    p( f(x) => v );
    wait;
  end process;
end architecture;
