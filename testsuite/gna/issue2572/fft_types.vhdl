library ieee;
use ieee.math_complex.all;
use std.textio.all;

package fft_types is
  subtype fft_data_type is complex;
  type fft_data_vector is array (natural range <>) of fft_data_type;
  pure function "+" (a, b : fft_data_vector) return fft_data_vector;
  pure function "*" (a, b : fft_data_vector) return fft_data_vector;
end package fft_types;

package body fft_types is
  pure function "+" (a, b : fft_data_vector) return fft_data_vector is
    variable res : fft_data_vector (a'range);
  begin
    for i in a'range loop
      res(i) := a(i) + b(i);
    end loop;
    return res;
  end function;

  pure function "*" (a, b : fft_data_vector) return fft_data_vector is
    variable res : fft_data_vector (a'range);
  begin
    for i in a'range loop
      res(i) := a(i) * b(i);
    end loop;
    return res;
  end function;
end package body fft_types;
