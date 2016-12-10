--
--  File Name:         ScoreBoardGenericPkg.vhd
--  Design Unit Name:  ScoreBoardGenericPkg
--  Revision:          STANDARD VERSION
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis          email:  jim@synthworks.com
--
--
--  Description:
--    Defines types and methods to implement a FIFO based Scoreboard
--    Defines type ScoreBoardPType
--    Defines methods for putting values the scoreboard
--
--  Developed for:
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--  Latest standard version available at:
--        http://www.SynthWorks.com/downloads
--
--  Revision History:
--    Date      Version      Description
--    12/2006:   2006.12     Initial revision
--    08/2010    2010.08     Added Tailpointer
--    05/2012    2012.05     Changed FIFO to store pointers to ExpectedType
--                           Allows usage of unconstrained arrays
--    08/2012    2012.08     Added Type and Subprogram Generics
--    08/2013    2013.08     Generics:  to_string replaced write, Match replaced check
--                           Added Tags - Experimental
--                           Added Array of Scoreboards
--    09/2013    2013.09     Added file handling, Check Count, Finish Status
--                           Find, Flush
--    06/2015    2015.06     Added Alerts, SetAlertLogID, Revised LocalPush, GetDropCount,
--                           Deprecated SetFinish and ReportMode - REPORT_NONE, FileOpen
--                           Deallocate, Initialized, Function SetName
--    09/2016    2016.07     Released as part of OSVVM

--
--
--  Copyright (c) 2006 - 2016 by SynthWorks Design Inc.  All rights reserved.
--
--  Verbatim copies of this source file may be used and
--  distributed without restriction.
--
--  This source file is free software; you can redistribute it
--  and/or modify it under the terms of the ARTISTIC License
--  as published by The Perl Foundation; either version 2.0 of
--  the License, or (at your option) any later version.
--
--  This source is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the Artistic License for details.
--
--  You should have received a copy of the license with this source.
--  If not download it from,
--     http://www.perlfoundation.org/artistic_license_2_0
--
--

use std.textio.all ;

library ieee ;
  use ieee.std_logic_1164.all ;
  use ieee.numeric_std.all ;

package ScoreboardGenericPkg is
  generic (
    type ExpectedType ;
    type ActualType ;
    function Match(Actual : ActualType ;                           -- defaults
                   Expected : ExpectedType) return boolean ;       -- is "=" ;
    function expected_to_string(A : ExpectedType) return string ;  -- is to_string ;
    function actual_to_string  (A : ActualType) return string      -- is to_string ;
  ) ;

--   --  For a VHDL-2002 package, comment out the generics and
--   --  uncomment the following, it replaces a generic instance of the package.
--   --  As a result, you will have multiple copies of the entire package.
--   --  Inconvenient, but ok as it still works the same.
--   subtype ExpectedType is std_logic_vector ;
--   subtype ActualType   is std_logic_vector ;
--   alias Match is std_match [ActualType, ExpectedType return boolean] ;  -- for std_logic_vector
--   alias expected_to_string is to_hstring [ExpectedType return string];  -- VHDL-2008
--   alias actual_to_string is to_hstring [ActualType return string];  -- VHDL-2008

  -- ScoreboardReportType is deprecated
  -- Replaced by Affirmations.  ERROR is the default.  ALL turns on PASSED flag
  type ScoreboardReportType is (REPORT_ERROR, REPORT_ALL, REPORT_NONE) ;   -- replaced by affirmations

  type ScoreBoardPType is protected

    ------------------------------------------------------------
    ------------------------------------------------------------
    -- Push items into the scoreboard/FIFO

    -- Simple Scoreboard, no tag
    procedure Push (Item   : in  ExpectedType) ;

  end protected ScoreBoardPType ;

end ScoreboardGenericPkg ;


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
package body ScoreboardGenericPkg is

  type ScoreBoardPType is protected body

    ------------------------------------------------------------
    -- Simple Scoreboard, no tag
    procedure Push (Item   : in  ExpectedType) is
    ------------------------------------------------------------
    begin
      null;
    end procedure Push ;

  end protected body ScoreBoardPType ;
end ScoreboardGenericPkg ;
