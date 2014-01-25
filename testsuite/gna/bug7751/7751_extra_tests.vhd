entity tb is
end tb;

architecture sim of tb is

  -- Extra tests for 'image and 'value on enumeration types (other than boolean)

  -- Type with less than 256 values
  type e8 is (one, two, three, four);

  -- Type with more than 256 values
  type e32 is (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9,
               T20, T21, T22, T23, T24, T25, T26, T27, T28, T29,
               T30, T31, T32, T33, T34, T35, T36, T37, T38, T39,
               T40, T41, T42, T43, T44, T45, T46, T47, T48, T49,
               T50, T51, T52, T53, T54, T55, T56, T57, T58, T59,
               T60, T61, T62, T63, T64, T65, T66, T67, T68, T69,
               T70, T71, T72, T73, T74, T75, T76, T77, T78, T79,
               T80, T81, T82, T83, T84, T85, T86, T87, T88, T89,
               T90, T91, T92, T93, T94, T95, T96, T97, T98, T99,
               T100, T101, T102, T103, T104, T105, T106, T107, T108, T109,
               T110, T111, T112, T113, T114, T115, T116, T117, T118, T119,
               T120, T121, T122, T123, T124, T125, T126, T127, T128, T129,
               T130, T131, T132, T133, T134, T135, T136, T137, T138, T139,
               T140, T141, T142, T143, T144, T145, T146, T147, T148, T149,
               T150, T151, T152, T153, T154, T155, T156, T157, T158, T159,
               T160, T161, T162, T163, T164, T165, T166, T167, T168, T169,
               T170, T171, T172, T173, T174, T175, T176, T177, T178, T179,
               T180, T181, T182, T183, T184, T185, T186, T187, T188, T189,
               T190, T191, T192, T193, T194, T195, T196, T197, T198, T199,
               T200, T201, T202, T203, T204, T205, T206, T207, T208, T209,
               T210, T211, T212, T213, T214, T215, T216, T217, T218, T219,
               T220, T221, T222, T223, T224, T225, T226, T227, T228, T229,
               T230, T231, T232, T233, T234, T235, T236, T237, T238, T239,
               T240, T241, T242, T243, T244, T245, T246, T247, T248, T249,
               T250, T251, T252, T253, T254, T255, T256, T257, T258, T259,
               T260, T261, T262, T263, T264, T265, T266, T267, T268, T269,
               T270, T271, T272, T273, T274, T275, T276, T277, T278, T279,
               T280, T281, T282, T283, T284, T285, T286, T287, T288, T289,
               T290, T291, T292, T293, T294, T295, T296, T297, T298, T299 );

-------------- static value ----------------------

  -- static value : enumeration
  constant e8str : string := "three";
  constant e8val : e8 := e8'value("TWO");

  constant e32str_1 : string := "T254";
  constant e32str_2 : string := "T257";
  constant e32val_1 : e32 := e32'value("T250");
  constant e32val_2 : e32 := e32'value("T260");

-------------- static image ----------------------

  -- static image : enumeration
  constant e8_img1 : string := e8'image(One);
  constant e8_img2 : string := e8'image(Two);

  constant e32_img1 : string := e32'image(T1);
  constant e32_img2 : string := e32'image(T299);


-------------- runtime value  ----------------------

  -- runtime enumeration
  signal my_e8 : e8 := One;
  signal my_e8_str  : string(1 to 3) := "Two";

  signal my_e32     : e32 := T298;
  signal my_e32_str : string(1 to 3) := "T22";

  function e_val (s : string) return e8 is
  begin
    return e8'value(s);
  end e_val;

  function e_val (s : string) return e32 is
  begin
    return e32'value(s);
  end e_val;

-------------- runtime image ----------------------
  -- runtime enumeration
  signal sig_e8  : e8 := e8'value("Three"); 
  signal sig_e32  : e32 := e32'value("T123"); 

  function e_img (e : e8) return string is
  begin
    return e8'image(e);
  end e_img;

  function e_img (e : e32) return string is
  begin
    return e32'image(e);
  end e_img;


  function t_val (t : string) return time is
    begin
      return time'value (t);
    end t_val;
begin
-- At least one test for each constant, signal or function

-- Value tests : static enumeration expressions.
  Assert e8'value("One") = Two report "Assertion 1 triggered ... correctly" severity NOTE;
  Assert e8'value("One") = One report "Assertion 2 triggered ... wrongly" severity FAILURE;
  Assert e8'value(e8str) = Two report "Assertion 3 triggered ... correctly" severity NOTE;
  Assert e8'value(e8str) = Three report "Assertion 4 triggered ... wrongly" severity FAILURE;

  Assert e8val = Four report "Assertion 5 triggered ... correctly" severity NOTE;
  Assert e8val = Two report "Assertion 6 triggered ... wrongly" severity FAILURE;

  Assert e32'value("T1") = T2 report "Assertion 7 triggered ... correctly" severity NOTE;
  Assert e32'value("T2") = T2 report "Assertion 8 triggered ... wrongly" severity FAILURE;
  Assert e32'value(e32str_1) = T257 report "Assertion 9 triggered ... correctly" severity NOTE;
  Assert e32'value(e32str_2) = T257 report "Assertion 10 triggered ... wrongly" severity FAILURE;
  Assert e32val_1 = T260 report "Assertion 11 triggered ... correctly" severity NOTE;
  Assert e32val_2 = T260 report "Assertion 12 triggered ... wrongly" severity FAILURE;

  -- static image : enumeration
  Assert e8_img1 = "One" report "Assertion 13 triggered ... correctly" severity NOTE;
  Assert e8_img2 = "two" report "Assertion 14 triggered ... wrongly" severity FAILURE;

  Assert e32_img1 = "T1" report "Assertion 15 triggered ... correctly" severity NOTE;
  Assert e32_img2 = "t299" report "Assertion 16 triggered ... wrongly" severity FAILURE;

-------------- runtime value  ----------------------
  my_e8 <= Two after 10 ns;
  my_e8_str <= "One" after 20 ns;
  Assert my_e8 = One report "Assertion 17 triggered ... correctly" severity NOTE;
  Assert my_e8 = Two report "Assertion 18 triggered ... correctly" severity NOTE;
  Assert e_val(my_e8_str) = Three report "Assertion 19 triggered ... correctly" severity NOTE;
  Assert e_val(my_e8_str) = Two report "Assertion 20 triggered ... wrongly except at 20ns" severity NOTE;

  my_e32 <= T297 after 30 ns;

  Assert my_e32 = T296 report "Assertion 21 triggered ... correctly" severity NOTE;

  my_e32_str <= "T24" after 40 ns;
  Assert e_val(my_e32_str) = T23 report "Assertion 22 triggered ... correctly" severity NOTE;
  Assert e_val(my_e32_str) = T22 report "Assertion 23 triggered ... wrongly except at 40ns" severity NOTE;

  --  Check white spaces and case.
  assert e_val(" one") = one report "assertion 31" severity failure;
  assert e_val(" one ") = one report "assertion 32" severity failure;
  assert e_val("one ") = one report "assertion 33" severity failure;
  assert e_val("oNe") = one report "assertion 34" severity failure;

  assert e_val(" T1") = t1 report "assertion 35" severity failure;
  assert e_val(" t2 ") = t2 report "assertion 36" severity failure;
  assert e_val("t3 ") = t3 report "assertion 37" severity failure;
  assert e_val("t39") = t39 report "assertion 38" severity failure;

  assert t_val("1 ns") = 1 ns report "assertion 40" severity failure;
  assert t_val(" 1 nS") = 1 ns report "assertion 41" severity failure;
  assert t_val(" 1 Ns ") = 1 ns report "assertion 42" severity failure;
  assert t_val(" -1.5 ns ") = -1500 ps report "assertion 44" severity failure;
  
-------------- runtime image ----------------------
  -- runtime enumeration
  sig_e8 <= Two after 50 ns, Four after 60 ns;
  Assert sig_e8 = One report "Sig_e8 = " & e8'image(sig_e8) & " fn returns " & e_img(sig_e8) severity Note;

  sig_e32 <= T124 after 70 ns, T125 after 80 ns;
  Assert sig_e32 = T122 report "Sig_e32 = " & e32'image(sig_e32) & " fn returns " & e_img(sig_e32) severity Note;

end sim; 
