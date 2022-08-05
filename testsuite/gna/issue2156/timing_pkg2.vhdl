package timing_pkg is

  type frequency is range 0.0 to real'high;

  function to_string(
    freq : frequency;
    unit : string := "";
    fmt  : string := "%.6f")
    return string;

  function to_string(value: frequency) return string;
end package;

package body timing_pkg is

  function to_string(
    freq : frequency;
    unit : string := "";
    fmt  : string := "%.6f")
    return string
  is
  begin
    if unit = "" then
      if freq < 1.0e3 then
        return to_string(real(freq), fmt) & " Hz";
      elsif 1.0e3 <= freq and freq < 1.0e6 then
        return to_string(real(freq) * 1.0e-3, fmt) & " kHz";
      elsif 1.0e6 <= freq and freq < 1.0e9 then
        return to_string(real(freq) * 1.0e-6, fmt) & " MHz";
      else
        return to_string(real(freq) * 1.0e-9, fmt) & " GHz";
      end if;
    elsif unit = "Hz" then
      return to_string(real(freq), fmt) & " Hz";
    elsif unit = "kHz" then
      return to_string(real(freq) * 1.0e-3, fmt) & " kHz";
    elsif unit = "MHz" then
      return to_string(real(freq) * 1.0e-6, fmt) & " MHz";
    elsif unit = "GHz" then
      return to_string(real(freq) * 1.0e-9, fmt) & " GHz";
    else
      report "invalid frequency unit (Hz, kHz, MHz, GHz)"
      severity failure;
    end if;
  end function;
  
  function to_string(value: frequency) return string is
  begin
      return to_string(value, "", "%.6f");
  end function;
end package body;
