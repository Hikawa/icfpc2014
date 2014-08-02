#!/usr/bin/awk -f

# icfpc2014: icfp contest 2014
# Copyright (C) 2014  The sound of lambda team
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

BEGIN{
  nextcmd = 0
  f=ARGV[1]
  while (getline <f) {
    i = index($0,";")
    if (i > 0)
      $0 = substr($0,1,i-1);
    while (substr($0,length($0),1) == " ")
      $0 = substr($0,1,length($0)-1);
    if ($0 == "")
      continue;
    if (substr($0,length($0),1) == ":")
      if ($0 in links)
        print "Link " $0 " duplicated!!!";
      else
        links[$0] = nextcmd;
    else
      nextcmd++;
  }
  close(f)
  nextcmd = 0
}
{
  i = index($0,";")
  if (i > 0)
    $0 = substr($0,1,i-1);
  while (substr($0,length($0),1) == " ")
    $0 = substr($0,1,length($0)-1);
  while (substr($0,1,1) == " ")
    $0 = substr($0,2);
  if ($0 == "")
    next;
  if (substr($0,length($0),1) == ":")
    links[$0] = nextcmd;
  else
  {
    lstart = index($0, "_")
    if (lstart > 0)
    { # have label, find and replace
      lend = lstart+1
      c = substr($0, lend, 1)
      while (c != "" && c != " " && c != ",")
        c = substr($0, ++lend, 1);
      id = substr($0, lstart, lend-lstart)
      $0 = substr($0, 1, lstart-1) links[id ":"] substr($0, lend)
    }
    print
    nextcmd++
  }
}
