------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                     P A R A L L E L . L O O P S .                   --
--                        W O R K _ S E E K I N G                           --
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

--  This package provides the capability to execute a loop in parallel
--  using a work seeking approach without producing a final result.

with System.Storage_Elements;
with Ada.Dynamic_Priorities; use Ada;

generic
package Parallel.Loops.Work_Seeking is

   type Work_Seeking_Manager
     (Chunk_Size : Natural := Default_Chunk_Size;
      Ceiling_Priority : System.Priority :=
        Dynamic_Priorities.Get_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size) is limited
     new Parallelism_Manager with null record;

   not overriding
   function Create
     (Chunk_Size : Natural := Default_Chunk_Size;
      Ceiling_Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size)
   return Work_Seeking_Manager;

   overriding
   procedure Execute_Parallel_Loop
     (Manager : Work_Seeking_Manager;

      From : Iteration_Index_Type := Iteration_Index_Type'First;
      To : Iteration_Index_Type := Iteration_Index_Type'Last;
      Worker_Count : Worker_Count_Type := Use_Optimal_Worker_Count;

      Process   : not null access procedure
        (Start, Finish : Iteration_Index_Type));

end Parallel.Loops.Work_Seeking;
