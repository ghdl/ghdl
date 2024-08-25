--  This file is part of OSVVM.
--  
--  Copyright (c) 2006 - 2021 by SynthWorks Design Inc.  
--  Copyright (C) 2021 by OSVVM Authors   
--  
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--  
--      https://www.apache.org/licenses/LICENSE-2.0
--  
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--  

library ieee ;
use ieee.math_real.all ;

package RandomPkg is
  type RandomPType is protected
    impure function Uniform (Min, Max : in real) return real ;
  end protected RandomPType ;
end RandomPkg ;

package body RandomPkg is
    function Scale (A, Min, Max : real) return real is
    begin
      return A * (max - min) + Min ;
    end function Scale ;

  type RandomPType is protected body
    impure function Uniform1 return real is 
      variable RandomSeed_l : integer := 1;
      variable RandomSeed_r : integer := 7;
      variable rRandom : real ; 
    begin
      ieee.math_real.Uniform (RandomSeed_l, RandomSeed_r, rRandom) ;
      return rRandom ; 
    end function Uniform1 ;

    impure function Uniform (Min, Max : in real) return real is
    begin
      return scale(Uniform1, Min, Max) ;
    end function Uniform ;
  end protected body RandomPType ;
end RandomPkg ;
