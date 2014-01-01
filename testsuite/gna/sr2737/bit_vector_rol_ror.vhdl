entity bit_vector_rol_ror is
end entity;

architecture ghdl_bug of bit_vector_rol_ror is

  function TO_STRING (VALUE : BIT_VECTOR) return STRING is
    alias ivalue : BIT_VECTOR(1 to value'length) is value;
    variable result : STRING(1 to value'length);
  begin
    if value'length < 1 then
      return "";
    else
      for i in ivalue'range loop
        if iValue(i) = '0' then
          result(i) := '0';
        else
          result(i) := '1';
        end if;
      end loop;
      return result;
    end if;
  end function to_string;

begin

  assert bit_vector'("11100") ror -8  = "00111" report "ror -8 is broken" severity warning;
  assert bit_vector'("11100") ror -7  = "10011" report "ror -7 is broken" severity warning;
  assert bit_vector'("11100") ror -6  = "11001" report "ror -6 is broken" severity warning;
  assert bit_vector'("11100") ror -5  = "11100" report "ror -5 is broken" severity warning;
  assert bit_vector'("11100") ror -4  = "01110" report "ror -4 is broken" severity warning;
  assert bit_vector'("11100") ror -3  = "00111" report "ror -3 is broken" severity warning;
  assert bit_vector'("11100") ror -2  = "10011" report "ror -2 is broken" severity warning;
  assert bit_vector'("11100") ror -1  = "11001" report "ror -1 is broken" severity warning;
  assert bit_vector'("11100") ror  0  = "11100" report "ror  0 is broken" severity warning;
  assert bit_vector'("11100") ror  1  = "01110" report "ror  1 is broken" severity warning;
  assert bit_vector'("11100") ror  2  = "00111" report "ror  2 is broken" severity warning;
  assert bit_vector'("11100") ror  3  = "10011" report "ror  3 is broken" severity warning;
  assert bit_vector'("11100") ror  4  = "11001" report "ror  4 is broken" severity warning;
  assert bit_vector'("11100"  ror  5)  = "11100" report "ror  5 is broken" severity warning;
  assert bit_vector'("11100") ror  5 = "11100" report string'("ror  5 is broken " & TO_STRING(bit_vector'("11100"))&" produces "& TO_STRING(bit_vector'("11100") ror 5) &"!") severity warning;
  assert bit_vector'("11100") ror  6  = "01110" report "ror  6 is broken" severity warning;
  assert bit_vector'("11100") ror  7  = "00111" report "ror  7 is broken" severity warning;
  assert bit_vector'("11100") ror  8  = "10011" report "ror  8 is broken" severity warning;

  assert bit_vector'("11100") rol -8  = "10011" report "rol -8 is broken" severity warning;
  assert bit_vector'("11100") rol -7  = "00111" report "rol -7 is broken" severity warning;
  assert bit_vector'("11100") rol -6  = "01110" report "rol -6 is broken" severity warning;
  assert bit_vector'("11100"  rol -5)  = "11100" report "rol -5 is broken" severity warning;
  assert bit_vector'("11100") rol -5 = "11100" report string'("rol -5 is broken " & TO_STRING(bit_vector'("11100"))&" produces "& TO_STRING(bit_vector'("11100") rol-5) &"!") severity warning;
  assert bit_vector'("11100") rol -4  = "11001" report "rol -4 is broken" severity warning;
  assert bit_vector'("11100") rol -3  = "10011" report "rol -3 is broken" severity warning;
  assert bit_vector'("11100") rol -2  = "00111" report "rol -2 is broken" severity warning;
  assert bit_vector'("11100") rol -1  = "01110" report "rol -1 is broken" severity warning;
  assert bit_vector'("11100") rol  0  = "11100" report "rol  0 is broken" severity warning;
  assert bit_vector'("11100") rol  1  = "11001" report "rol  1 is broken" severity warning;
  assert bit_vector'("11100") rol  2  = "10011" report "rol  2 is broken" severity warning;
  assert bit_vector'("11100") rol  3  = "00111" report "rol  3 is broken" severity warning;
  assert bit_vector'("11100") rol  4  = "01110" report "rol  4 is broken" severity warning;
  assert bit_vector'("11100") rol  5  = "11100" report "rol  5 is broken" severity warning;
  assert bit_vector'("11100") rol  6  = "11001" report "rol  6 is broken" severity warning;
  assert bit_vector'("11100") rol  7  = "10011" report "rol  7 is broken" severity warning;
  assert bit_vector'("11100") rol  8  = "00111" report "rol  8 is broken" severity warning;

end architecture;