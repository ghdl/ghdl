package nullrng is

  type my_time is range -integer'low  to integer'high units
    fs; ps= 1000 fs; ns= 1000 ps; us= 1000 ns; -- very short
    ms= 1000 us; sec= 1000 ms; min= 60 sec; hr= 60 min; -- longer
  end units;
  type my_empty_range is range -(-8) to 7;
  type my_small_range is range -8 to 7;

end;
