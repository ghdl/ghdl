entity repro1 is
end repro1;

architecture arch of repro1 is
  type wf_el is record
    t : time;
    v : bit;
  end record;

  type wf_arr is array (natural range <>) of wf_el;

  function get_wf (bv : bit_vector; p : time) return wf_arr is
    variable res : wf_arr (bv'range);
    variable t : time;
  begin
    t := 0 ns;
    for i in bv'range loop
      res (i) := (t => t, v => bv (i));
      t := t + p;
    end loop;
    return res;
  end get_wf;

  procedure play_wf (signal s : out bit; wf : wf_arr; init : bit) is
  begin
    s <= init;
    for i in wf'range loop
      wait for wf (i).t;
      s <= wf (i).v;
    end loop;
    wait;
  end play_wf;

  function get_str (l : natural; c : character) return string is
  begin
    return string'(1 to l => c);
  end get_str;
  
  signal o : bit;
begin
  play_wf (o, get_wf (b"0110100", 2 ns), '1');

  process
  begin
    for i in 1 to 8 loop
      report get_str (32 + 4 * i, character'val (64 + i));
      wait for 2 ns;
    end loop;
    wait;
  end process;
end arch;
