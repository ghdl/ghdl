/*  Declarations to interface with Ada code.
    Copyright (C) 2015 Tristan Gingold.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <gnu.org/licenses>.

    As a special exception, if other files instantiate generics from this
    unit, or you link this unit with other files to produce an executable,
    this unit does not by itself cause the resulting executable to be
    covered by the GNU General Public License. This exception does not
    however invalidate any other reasons why the executable file might be
    covered by the GNU Public License.
*/

struct backtrace_addrs
{
  int size;
  int skip;
  void *addrs[32];
};

void grt_save_backtrace (struct backtrace_addrs *bt, int skip);

extern void grt_overflow_error (struct backtrace_addrs *bt);
extern void grt_null_access_error (struct backtrace_addrs *bt);

void __ghdl_maybe_return_via_longjump (int val);
int __ghdl_run_through_longjump (int (*func)(void));
