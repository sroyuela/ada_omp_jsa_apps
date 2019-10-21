------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L .
--                           R E C U R S I O N .
--                       W O R K _ S H A R I N G
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

--  This package provides the capability to recurse in parallel
--  using a work sharing approach without producing a final result

with System.Storage_Elements;
private with Ada.Task_Attributes;

generic
package Parallel.Recursion.Work_Sharing is

   type Recursion_Branch_Count is range 1 .. 2**8 - 1;

   type Recursion_Dispatcher is limited interface;

   procedure Recurse
     (Dispatcher : Recursion_Dispatcher;
      Item : Work_Type;
      Split : Recursion_Branch_Count;
      Of_Splits : Recursion_Branch_Count)
   is abstract;

   type Recursion_Dispatcher_Access is access all Recursion_Dispatcher'Class;

   type Work_Sharing_Manager
     (Ceiling_Priority : System.Priority := System.Default_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size)
   is limited new Parallelism_Manager with
      record
         Dispatcher : aliased Recursion_Dispatcher_Access
           := null;
      end record;

   not overriding
   function Create
     (Ceiling_Priority : System.Priority := System.Default_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size)
      return Work_Sharing_Manager;

   not overriding
   function Sequential
     (Manager : Work_Sharing_Manager)
     return Boolean;

private

   pragma Warnings (Off, "*postcondition does not mention result*");

   overriding
   procedure Execute_Parallel_Subprogram
     (Manager : in out Work_Sharing_Manager;
      Item : Work_Type;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process : not null access procedure (Item : Work_Type))
   with Pre'Class => Manager.Dispatcher = null,
        Post'Class => Manager.Dispatcher = null;

   pragma Warnings (On, "*postcondition does not mention result*");

   package Parallel_Task_Attributes is new Ada.Task_Attributes
     (Attribute     => Worker_Count_Type,
      Initial_Value => Worker_Count_Type'First);

   function Sequential
     (Manager : Work_Sharing_Manager)
      return Boolean
   is (Parallel_Task_Attributes.Value <= 1);

   pragma Inline (Sequential);

end Parallel.Recursion.Work_Sharing;
