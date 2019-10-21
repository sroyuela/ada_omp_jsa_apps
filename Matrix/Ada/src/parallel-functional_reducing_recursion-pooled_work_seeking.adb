------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                             P A R A L L E L .
--                F U N C T I O N A L _ R E D U C T I O N .
--                            R E C U R S I O N .
--                  P O O L E D _ W O R K _ S E E K I N G
--                                                                          --
--                                B o d y                                   --
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

with System.Multiprocessors.Dispatching_Domains;
with Ada.Exceptions; use Ada;

package body Parallel.Functional_Reducing_Recursion.Pooled_Work_Seeking is

   function Create
     (Others_Seeking_Work : aliased in out Work_Seeking_State;
      Workers : not null access Task_Pools.Task_Pool_Interface'Class
      := Parallel.Task_Pools.Default_Task_Pool;
      Ceiling_Priority : System.Priority := System.Default_Priority;
      Affinity : access Dispatching_Domains.CPU_Set := null;
      Allow_Migration : Boolean := True)
      return Work_Seeking_Manager is
   begin
      return Manager : Work_Seeking_Manager
        (Workers,
         Others_Seeking_Work'Access,
         Ceiling_Priority,
         Affinity,
         Allow_Migration)
      do
         null;
      end return;

   end Create;

   function Execute_Parallel_Subprogram
     (Manager : in out Work_Seeking_Manager;
      Item : Work_Type;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process : not null access function (Item : Work_Type) return Result_Type)
      return Result_Type
   is
      type Work_Seeking_Plan is
        new Parallel.Task_Pools.Work_Plan with null record;

      overriding
      procedure Engage (Plan : Work_Seeking_Plan);

      protected type Work_Seeker is

         pragma Priority (Manager.Ceiling_Priority);

         entry Wait_For_Work
           (Item : out Work_Type;
            Done : out Boolean);

         procedure Offer_Work
           (Item          : Work_Type;
            Work_Accepted : out Boolean);

         entry Wait_For_Worker_Initialization
           (Item          : Work_Type);

         entry Wait_For_Completion
           (Final_Result : out Result_Type);

         procedure Reduce (Value : Result_Type);

         entry Request_Work
           (Item : out Work_Type;
            Done : out Boolean);

         procedure Master_Finished;

         procedure Save_Exception (E : Exceptions.Exception_Occurrence);

      private
         Unemployed_Workers : Worker_Count_Type := 0;
         Work_Offered       : Boolean           := False;
         All_Work_Complete  : Boolean := False;
         Master_Is_Done     : Boolean := False;
         Assignment_Value   : Work_Type;
         Result : Result_Type := Identity_Value;
         Final_Reduction_Count : Worker_Count_Type := 0;
         Exception_Raised    : Boolean := False;
         Saved_Exception     : Ada.Exceptions.Exception_Occurrence;

      end Work_Seeker;

      protected body Work_Seeker is

         procedure Master_Finished is
         begin
            Master_Is_Done := True;
            if Unemployed_Workers = Worker_Count then
               All_Work_Complete := True;
            end if;
         end Master_Finished;

         procedure Offer_Work
           (Item : Work_Type;
            Work_Accepted : out Boolean) is
         begin
            --  Others looking for work
            if Wait_For_Work'Count > 0 and then not All_Work_Complete then

               Work_Offered := True;
               Assignment_Value := Item;

               Work_Accepted := True;

               Manager.Other_Workers.all
                 := (Seeking_Work => Wait_For_Work'Count > 1);
            else
               --  Nobody looking for work or no work to offer
               Work_Accepted := False;
            end if;
         end Offer_Work;

         procedure Reduce (Value : Result_Type) is
         begin
            Result := Reducer (Result, Value);
            Final_Reduction_Count := Final_Reduction_Count + 1;
         end Reduce;

         entry Request_Work
           (Item : out Work_Type;
            Done : out Boolean) when Standard.True is
         begin
            Unemployed_Workers := Unemployed_Workers + 1;

            if Unemployed_Workers = Worker_Count
              and then Master_Is_Done then
               All_Work_Complete := True;
               Done := True;
               return;
            else
               Done := False;
            end if;

            Manager.Other_Workers.all := (Seeking_Work => True);

            requeue Wait_For_Work;
         end Request_Work;

         procedure Save_Exception (E : Exceptions.Exception_Occurrence) is
         begin
            if not Exception_Raised then
               Exception_Raised := True;
               Ada.Exceptions.Save_Occurrence (Target => Saved_Exception,
                                               Source => E);
            end if;

            Unemployed_Workers := Unemployed_Workers + 1;
         end Save_Exception;

         entry Wait_For_Completion
           (Final_Result : out Result_Type)
              when Final_Reduction_Count = Worker_Count is
         begin
            Final_Result := Result;

            if Exception_Raised then
               Ada.Exceptions.Reraise_Occurrence (X => Saved_Exception);
            end if;
         end Wait_For_Completion;

         entry Wait_For_Work
           (Item : out Work_Type;
            Done : out Boolean)
           when Work_Offered or else All_Work_Complete is
         begin
            if All_Work_Complete then
               Done := True;
            else
               Done := False;
               Unemployed_Workers := Unemployed_Workers - 1;
               Item := Assignment_Value;

               Work_Offered := False;
            end if;
         end Wait_For_Work;

         entry Wait_For_Worker_Initialization
           (Item          : Work_Type)
           when Wait_For_Work'Count = Worker_Count
         is
            Dont_Care : Boolean;
         begin
            Offer_Work (Item, Dont_Care);
         end Wait_For_Worker_Initialization;

      end Work_Seeker;

      --  Allow client to call recursion routine
      Scheduler : Work_Seeker;

      procedure Engage
        (Plan : Work_Seeking_Plan)
      is
         pragma Unreferenced (Plan);

         Value : Work_Type;
         Done : Boolean := False;
         Result : Result_Type := Identity_Value;

      begin -- Engage

         if not Manager.Allow_Migration then
            --  Fix. Need to figure out which CPU to
            --  specify based on Affinity discriminant and
            --  task id number.
            System.Multiprocessors.Dispatching_Domains.Set_CPU (CPU => 1);
         elsif Manager.Affinity /= null then
            Parallel.Dispatching_Domains.Set_CPU (Set => Manager.Affinity.all);
         end if;

         Work_Loop : loop

            --  Execution completed, look for work
            --  from other tasks if possible
            Scheduler.Request_Work
              (Item => Value,
               Done => Done);

            exit Work_Loop when Done;

            Result := Reducer (Left => Result,
                               Right => Process (Value));

         end loop Work_Loop;

         Scheduler.Reduce (Value => Result);

         if not Manager.Allow_Migration or else
           Manager.Affinity /= null then
            System.Multiprocessors.Dispatching_Domains.Set_CPU
              (CPU => System.Multiprocessors.Not_A_Specific_CPU);
         end if;

      exception
         when E : others =>
            Scheduler.Save_Exception (E);
      end Engage;

      type Internal_Dispatcher_Type is new
        Recursion_Dispatcher with null record;

      overriding
      function Recurse
        (Dispatcher : Internal_Dispatcher_Type;
         Item : Work_Type) return Result_Type
      is
         pragma Unreferenced (Dispatcher);

         Work_Accepted : Boolean := False;
      begin

         if Manager.Other_Workers.Seeking_Work then

            Scheduler.Offer_Work
              (Item,
               Work_Accepted);

            if Work_Accepted then
               return Identity_Value;
            end if;  --  Work_Accepted
         end if; --  Workers_Are_Idle ...

         return Process (Item);
      end Recurse;

      Result : Result_Type;
      Plan : aliased Work_Seeking_Plan;
      Internal_Dispatcher : aliased Internal_Dispatcher_Type;

   begin -- Execute_Parallel_Subprogram

      --  Allow client to call recursion routine
      Manager.Dispatcher := Internal_Dispatcher'Unchecked_Access;

      --  Get the workers needed to do the work from the task pool
      Manager.Workers.Offer_Work (Item          => Plan,
                                  Worker_Count  => Worker_Count);

      Scheduler.Wait_For_Worker_Initialization (Item);

      Scheduler.Master_Finished;

      Scheduler.Wait_For_Completion (Result);

      Manager.Dispatcher := null;

      return Result;
   end Execute_Parallel_Subprogram;

end Parallel.Functional_Reducing_Recursion.Pooled_Work_Seeking;
