entity bug is
end entity bug;

architecture bug of bug is
  signal uninitialized_real:real; -- yea--this is poor coding practice
begin
  process
    begin
      report "bug="&real'image(uninitialized_real); -- prints out initial value -1.797693134862316e308
      
      wait for 1 ns;
      
      uninitialized_real<=2.0*uninitialized_real; -- result is apparently IEEE Inf
      wait for 0 ns; -- delta cycle to let new value take.
      
      report "bug="&real'image(uninitialized_real); -- this line never completes
      -- gets stuck in an infinite loop in grt.vstrings.to_string.  Relevant source file: grt-vstrings.adb
      -- Apparent reason for infinite loop--routine cannot determine exponent because the argument is Inf.
      wait;
    end process;
end architecture;
