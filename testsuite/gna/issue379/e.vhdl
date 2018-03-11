entity e is end entity;
architecture a of e is
begin
  process
    constant z :integer := 0;
    type t is array(0 to 0) of bit;
    procedure x(
      z :out bit_vector(0 to 0);
      f :out bit_vector(0 to 0)
    ) is
    begin
    end procedure;
    procedure x(
      z :out t;
      f :out bit_vector(0 to 0)
    ) is
    begin
    end procedure;
    function f(arg:t) return bit is
    begin
    end function;
    variable actual_for_f :bit;
    variable actual_for_z :t; -- bit
  begin
    x(
      f(z) => actual_for_f,
      f(z) => actual_for_z
    );
  end process;
end architecture;
