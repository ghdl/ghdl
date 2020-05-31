entity atod is
end atod;

architecture behav of atod is
  type real_array is array (natural range <>) of real;
  constant csts : real_array :=
    (1.0,
     0.0,
     --  Corner cases from
     --  http://www.exploringbinary.com/
     --    decimal-to-realing-point-needs-arbitrary-precision/

     7.8459735791271921e65,
     --  In binary:
     --  1.11011100 11010000 00001000 10011100 00010011 00010100 1110 e218
     --  1.   d   c    d   0    0   8    9   c    1   3    1   4    e

     3.571e266,
     --  1.01100010 01100100 01001100 01100001 11010100 00011010 1010 e885
     --  1.   6   2    6   4    4   c    6   1    d   4    1   a    a

     3.08984926168550152811E-32,
     --  1.01000000 11011110 01001000 01100111 01100110 01010011 1011 e-105
     --  1.   4   0    d   e    4   8    6   7    6   6    5   3    b

     7.4505805969238281e-09
     --  1.00000000 e-27
     );

begin
  process
    variable v : real;
  begin
    for i in csts'range loop
      report to_string (csts (i), "%a") severity note;
    end loop;

    --  There are two possible outputs according to the normalization of
    --  the first digit.
    v := csts (2);
    assert
         to_string (v, "%.13a") = "0x1.dcd0089c1314ep+218"
      or to_string (v, "%.13a") = "0xe.e68044e098a70p+215"
      severity failure;

    v := csts (3);
    assert
         to_string (v, "%.13a") = "0x1.62644c61d41aap+885"
      or to_string (v, "%.13a") = "0xb.1322630ea0d50p+882"
      severity failure;
    wait;
  end process;
end behav;
