library IEEE;
context IEEE.IEEE_std_context;

library work;
use work.my_fixed_pkg.all;

entity tb_fixed2 is
end;

architecture arch of tb_fixed2 is

begin

  process
    constant ref    : real := -9.96484375;

    subtype stype is sfixed(7 downto -8);

    -- Subtype not allowed as size_res argument of to_sfixed:
    -- constant input  : std_logic_vector (stype'length-1 downto 0) := to_slv(to_sfixed(ref, stype));
    -- Therefore, a variable needs to be created:
    variable fmt : stype;
    constant input  : std_logic_vector (stype'length-1 downto 0) := to_slv(to_sfixed(ref, fmt));

    variable sfmt : fmt'subtype;

    procedure report_sfixed(arg: sfixed) is begin report to_string(to_real(arg)); end procedure;

  begin
    report_sfixed(stype(input));
    report_sfixed(stype(signed(input)));

    -- CRASH
    report_sfixed(fmt'subtype(signed(input)));

    -- However, sfmt, which is declared using fmt'subtype, does work
    sfmt := stype(input);
    report_sfixed(sfmt);

    wait;
  end process;

end;
