library ieee;
use ieee.math_real.all;
use ieee.math_complex.all;
use work.fft_types.all;
use std.textio.all;

entity tb is end entity;

architecture mixed of tb is
  constant POW : natural := 2;
  constant DIM : positive := 2**POW;
  signal x, y : fft_data_vector (0 to DIM-1);

  function weight(n : positive) return fft_data_vector is
    variable w : fft_data_vector (0 to n-1);
  begin
    for k in w'range loop
      w(k) := exp(math_cbase_j*math_2_pi*real(k)/real(n));
    end loop;
    return w;
  end function;

begin

  x <= (complex'(0.0,0.0), complex'(1.0,1.0), complex'(2.0,2.0), complex'(3.0,3.0));

  fft_inst: entity work.fft(recursive)
    generic map (POW => POW)
    port map (x => x, w => weight(DIM), y => y);

  process (y) is
    variable lin : line;
    alias s is to_string [integer return string];
    alias s is to_string [real return string];
  begin
    for i in x'range loop
      lin := new string'("x(" & s(i) & ") = (" & s(x(i).re) & "," & s(x(i).im) & "), y(" & s(i) & ") = (" & s(y(i).re) & "," & s(y(i).im) & ")");
      writeline(output, lin);
      deallocate(lin);
    end loop;
  end process;

end architecture mixed;
