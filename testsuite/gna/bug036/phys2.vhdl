package phys2 is
  subtype mybv is bit_vector (0 to 3600000);

  type ANGLE is range mybv'range units
    sec;
    min = 60 sec;
    deg = 60 min;
  end units;
end phys2;
