entity top is 
end top;

architecture sim of top is

-------------- static value ----------------------

  -- static value : enumeration
  constant boolstr : string := "false";
  constant off : boolean := boolean'value("FALSE");
  -- static value : integer
  constant numstr : string := "5";
  -- static value : float
  constant fpstr1 : string := "123.4567";
  constant fpstr2 : string := "123.4567e-3";
  constant fpstr3 : string := "-123.4567e4";

  constant fp0 : real := real'value("123.4567");
  constant fp1 : real := real'value(fpstr1);
  constant fp2 : real := real'value(fpstr2);
  constant fp3 : real := real'value(fpstr3);

  -- static value : physical
  constant t_val_static : time :=  time'value("123 ns"); 

-------------- static image ----------------------

  -- static image : enumeration
  constant bool_img1 : string := boolean'image(False);
  constant bool_img2 : string := boolean'image(True);

  -- static image : integer
  constant int_img : string := integer'image(123);

  -- static image : float
  constant fpimg0 : string := real'image(fp0);
  constant fpimg1 : string := real'image(fp1);
  constant fpimg2 : string := real'image(fp2);
  constant fpimg3 : string := real'image(fp3);

  constant t_img_static : string := time'image(456 ps);
  -- physical types always evaluated at runtime...

-------------- runtime value  ----------------------
  -- runtime integer
  signal my_int : integer := 5;
  signal my_str1 : string(1 to 1) := "5";
  -- runtime boolean
  signal my_bool : boolean := true;

  -- runtime float
  signal my_flt : real := 0.0;

-------------- runtime image ----------------------
  -- runtime(signal) physical
  signal t : time := time'value("789 US"); 

  function t_img (t : time) return string is
  begin
    return time'image(t);
  end t_img;


begin
-- Value tests : static enumeration expressions.
  Assert boolean'value("FALSE") report "Bool Assertion triggered" severity NOTE;
  Assert boolean'value(boolstr) report "Bool Assertion triggered" severity NOTE;
-- Value tests : static integer expressions.
  Assert 2 + 2 = natural'value("5") report "Integer Assertion triggered" severity NOTE;
  Assert 2 + 2 = natural'value(numstr) report "Integer Assertion triggered" severity NOTE;
-- Value tests : static real expressions.
  Assert false report "real'value(""123.4567"" = " & real'image(fp0) severity NOTE;
-- Value tests : static physical expressions. Use time and at least one other phys unit.
  Assert false report "123 ns is " & time'image(t_val_static) severity note;
--  To check compiler error diagnosis, uncomment these.
--  Assert boolean'value(79) report "Assertion triggered" severity NOTE;
--  Assert boolean'value(False) report "Assertion triggered" severity NOTE;
--  Assert boolean'value("SILLY") report "Assertion triggered" severity NOTE;


-- Image tests : static enumeration expressions.
  Assert false report "Boolean can be " & boolean'image(True) & " or " & boolean'image(False) severity Note;
  Assert false report "Static Boolean can be " & bool_img1 & " or " & bool_img2 severity Note;
-- Image tests : static integer expressions.
  Assert false report "Integer image of 123 is " & int_img severity note;
-- Image tests : static real expressions.
  Assert false report "123.4567" & " = " & fpimg0 severity note;
  Assert false report "123.4567" & " = " & real'image(fp0) severity note;
  Assert false report "124.4567" & " = " & real'image(fp0 + 1.0) severity note;
  -- These assert despite nominally equal values.
  Assert fp0 = real'value(fpimg0) report "123.4567" & " = " & fpimg0 severity note;
  Assert fp1 = real'value(fpimg1) report fpstr1 & " = " & fpimg1 severity note;
  Assert fp2 = real'value(fpimg2) report fpstr2 & " = " & fpimg2 severity note;
  Assert fp3 = real'value(fpimg3) report fpstr3 & " = " & fpimg3 severity note;
  -- So verify that the differences are not actually 0
  Assert false report "fp0 - real'value(fpimg0) = " & real'image(fp0 - real'value(fpimg0)) severity note;
  Assert false report "fp1 - real'value(fpimg1) = " & real'image(fp1 - real'value(fpimg1)) severity note;
  Assert false report "fp2 - real'value(fpimg2) = " & real'image(fp2 - real'value(fpimg2)) severity note;
  Assert false report "fp3 - real'value(fpimg3) = " & real'image(fp3 - real'value(fpimg3)) severity note;
  -- Image tests : static physical expressions
  Assert false report "456 ps is " & t_img_static severity note;

-- Value tests : runtime expressions
  Assert boolean'value("FALSE") report "Assertion triggered" severity NOTE;
  Assert boolean'value(boolstr) report "Assertion triggered" severity NOTE;
  Assert my_bool report "Boolean my_bool = " & boolean'image(my_bool) severity NOTE;

  my_str1(1) <= '6' after 1 ns, '4' after 2 ns;
  my_flt <= fp0 after 3 ns;
  my_bool <= False after 4 ns;
  Assert my_flt = 0.0 report "my_flt = " & real'image(my_flt) severity note;
  Assert 2 + 2 = natural'value(my_str1) report "RT Assertion 1 triggered" severity NOTE;
  Assert 2 + 2 /= natural'value(my_str1) report "RT Assertion 2 triggered" severity NOTE;
  Assert my_bool report "Boolean my_bool = " & boolean'image(my_bool) severity NOTE;

-- Image tests : runtime physical expressions.
  Assert false report "Time " & t_img(123 us) severity note;

end sim; 
