------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      P A R A L L E L . L O O P S                   --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore                    --
--                                                                          --
--  Paraffin is free software;  you can  redistribute it  and/or modify it  --
--  under  terms of the  GNU General Public License  as  published  by the  --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later  version.  Paraffin is  distributed in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU  --
--  General Public License for  more details.  You should have  received a  --
--  copy of the GNU General Public License distributed with Paraffin;  see  --
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,   --
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.         --
--                                                                          --
--  As a  special exception, if other files  instantiate generics from      --
--  this unit,  or you link this  unit with other files  to produce an      --
--  executable,  this unit  does  not by  itself  cause the  resulting      --
--  executable to be covered by  the GNU General Public License.  This      --
--  exception does  not however invalidate  any other reasons  why the      --
--  executable file might be covered by the GNU Public License.             --
------------------------------------------------------------------------------
--  Generic to use for parallel loops that do not generate a final result

generic

   type Iteration_Index_Type is (<>);
   --  Loop Index type

package Parallel.Loops is

   type Parallelism_Manager is limited interface;

   procedure Execute_Parallel_Loop
     (Manager : Parallelism_Manager;
      From : Iteration_Index_Type := Iteration_Index_Type'First;
      To : Iteration_Index_Type := Iteration_Index_Type'Last;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process   : not null access procedure
        (Start, Finish : Iteration_Index_Type))
   is abstract;

end Parallel.Loops;
