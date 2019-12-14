use work.bug_pkg.t_zero_one;            -- .all works
entity bug is
  generic (
    test : t_zero_one := zero
    );
end entity bug;
