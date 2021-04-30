/* Display a GHDL Wavefile for debugging.
  Copyright (C) 2005 Tristan Gingold

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
*/

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "libghw.h"

static const char *progname;
void
usage (void)
{
  printf ("usage: %s [OPTIONS] FILEs...\n", progname);
  printf ("Options are:\n"
	  " -t  display types\n"
	  " -h  display hierarchy\n"
	  " -H  display hierarchy with full pathnames\n"
	  " -T  display time\n"
	  " -s  display signals (and time)\n"
	  " -S  display strings\n"
	  " -f  <lst> list of signals to display (default: all, example: -f "
	  "1,3,5-7,21-33)\n"
	  " -l  display list of sections\n" " -v  verbose\n");
}

static void
add_single_signal (int **signalSet, int *nbSignals, int signal)
{
  assert (NULL != signalSet);
  assert (NULL != nbSignals);
  assert (0 <= nbSignals[0]);
  assert (0 <= signal);

  int newSize = (1 + nbSignals[0]);
  /*printf("adding signal %6d set of signals to display\n", signal); */
  signalSet[0] = (int *) realloc (signalSet[0], newSize * sizeof (int));
  signalSet[0][nbSignals[0]] = signal;
  nbSignals[0] = newSize;
}

static void
add_signal_range (int **signalSet, int *nbSignals, const char *s,
		  const char *e)
{

  int i;
  int rangeSize;
  int rangeEnd = -1;
  int rangeStart = -1;
  int bytesMatched = -1;
  int expected = ((e - s) - 1);
  int itemsMatched =
    sscanf (s, "%d-%d%n", &rangeStart, &rangeEnd, &bytesMatched);
  if (2 == itemsMatched && expected == bytesMatched)
    {
      if (rangeEnd < rangeStart)
	{
	  int t = rangeEnd;
	  rangeEnd = rangeStart;
	  rangeStart = t;
	}
    }
  else
    {
      itemsMatched = sscanf (s, "%d%n", &rangeStart, &bytesMatched);
      if (1 == itemsMatched && expected == bytesMatched)
	{
	  if (0 <= rangeStart)
	    {
	      rangeEnd = rangeStart;
	    }
	}
    }

  rangeSize = (rangeEnd - rangeStart);
  if (rangeEnd < 0 || rangeStart < 0 || rangeSize < 0)
    {
      fprintf (stderr,
	       "incorrect signal range specification\"%s\" found in "
	       "command line, aborting\n", s);
      exit (1);
    }

  for (i = rangeStart; i <= rangeEnd; ++i)
    {
      add_single_signal (signalSet, nbSignals, i);
    }
}

static void
add_signals (int **signalSet, int *nbSignals, const char *arg)
{
  int c = -1;
  const char *e;
  const char *s = e = arg;
  while (0 != c)
    {
      c = *(e++);
      if (',' == c || 0 == c)
	{
	  add_signal_range (signalSet, nbSignals, s, e);
	  s = e;
	}
    }
}

static void
disp_string_table (struct ghw_handler *hp)
{
  int i;
  printf ("String table:\n");

  for (i = 1; i < hp->nbr_str; i++)
    printf (" %s\n", hp->str_table[i]);
}

int
main (int argc, char **argv)
{
  int i;
  int flag_disp_types;
  int flag_disp_hierarchy;
  int flag_disp_time;
  int flag_disp_signals;
  int flag_disp_strings;
  int flag_full_names;
  int flag_list;
  int flag_verbose;
  int nb_signals;
  int *signal_set;
  int filter_done;
  int eof;
  enum ghw_sm_type sm;

  progname = argv[0];
  flag_disp_types = 0;
  flag_disp_hierarchy = 0;
  flag_full_names = 0;
  flag_disp_time = 0;
  flag_disp_signals = 0;
  flag_disp_strings = 0;
  flag_list = 0;
  flag_verbose = 0;
  nb_signals = 0;
  signal_set = NULL;
  filter_done = 0;

  /* Disp help if there is only an help option.  */
  if (argc == 2
      && (strcmp (argv[1], "--help") == 0 || strcmp (argv[1], "-h") == 0))
    {
      usage ();
      exit (0);
    }

  while (1)
    {
      int c;

      c = getopt (argc, argv, "thHTsSlvf:");
      if (c == -1)
	break;
      switch (c)
	{
	case 't':
	  flag_disp_types = 1;
	  break;
	case 'h':
	  flag_disp_hierarchy = 1;
	  break;
	case 'H':
	  flag_disp_hierarchy = 1;
	  flag_full_names = 1;
	  break;
	case 'T':
	  flag_disp_time = 1;
	  break;
	case 's':
	  flag_disp_signals = 1;
	  flag_disp_time = 1;
	  break;
	case 'S':
	  flag_disp_strings = 1;
	  break;
	case 'f':
	  add_signals (&signal_set, &nb_signals, optarg);
	  break;
	case 'l':
	  flag_list = 1;
	  break;
	case 'v':
	  flag_verbose++;
	  break;
	default:
	  usage ();
	  exit (2);
	}
    }

  if (optind >= argc)
    {
      usage ();
      return 1;
    }

  for (i = optind; i < argc; i++)
    {
      struct ghw_handler h;
      struct ghw_handler *hp = &h;

      hp->flag_verbose = flag_verbose;

      if (ghw_open (hp, argv[i]) != 0)
	{
	  fprintf (stderr, "cannot open ghw file %s\n", argv[i]);
	  return 1;
	}
      if (flag_list)
	{
	  while (1)
	    {
	      const char *section_name;
	      int section;

	      section = ghw_read_section (hp);
	      if (section == -2)
		{
		  printf ("eof of file\n");
		  break;
		}
	      else if (section < 0)
		{
		  printf ("Error in file\n");
		  break;
		}
	      else if (section == 0)
		{
		  printf ("Unknown section\n");
		  break;
		}
	      section_name = ghw_sections[section].name;
	      printf ("Section %s\n", section_name);
	      if ((*ghw_sections[section].handler) (hp) < 0)
		break;

	      if (flag_disp_strings && strcmp (section_name, "STR") == 0)
		disp_string_table (hp);
	      else if (flag_disp_types && strcmp (section_name, "TYP") == 0)
		ghw_disp_types (hp);
	    }
	}
      else
	{
	  if (ghw_read_base (hp) < 0)
	    {
	      fprintf (stderr, "cannot read ghw file\n");
	      return 2;
	    }
	  if (flag_disp_types)
	    ghw_disp_types (hp);
	  if (flag_disp_hierarchy)
	    {
	      hp->flag_full_names = flag_full_names;
	      ghw_disp_hie (hp, hp->hie);
	    }

#if 1
	  sm = ghw_sm_init;
	  eof = 0;
	  while (!eof)
	    {
	      switch (ghw_read_sm (hp, &sm))
		{
		case ghw_res_snapshot:
		case ghw_res_cycle:
		  if (flag_disp_time)
		    printf ("Time is " GHWPRI64 " fs\n", hp->snap_time);
		  if (flag_disp_signals)
		    {
		      if (!filter_done)
			{
			  ghw_filter_signals (hp, signal_set, nb_signals);
			  filter_done = 1;
			}
		      ghw_disp_values (hp);
		    }
		  break;
		case ghw_res_eof:
		  eof = 1;
		  break;
		default:
		  abort ();
		}
	    }

#else
	  if (ghw_read_dump (hp) < 0)
	    {
	      fprintf (stderr, "error in ghw dump\n");
	      return 3;
	    }
#endif
	}
      ghw_close (&h);
    }
  return 0;
}
