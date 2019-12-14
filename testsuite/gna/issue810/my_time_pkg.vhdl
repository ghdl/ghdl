package my_time_pkg is

  type my_time is range -integer'low  to integer'high units
    fs;
    ps = 1000 fs;
    ns = 1000 ps;
    us = 1000 ns;
    ms = 1000 us;
    sec = 1000 ms;
    min = 60 sec;
    hr = 60 min;
  end units;

end package my_time_pkg;
