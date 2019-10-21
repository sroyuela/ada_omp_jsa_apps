pragma Profile (Ravenscar);

package Parallel.Ravenscar_Task_Pools is

--   pragma Preelaborate;

   --  A Pool_Index identifies a worker within the Task Pool
   type Pool_Worker_Count is new Worker_Count_Type;
   subtype Pool_Index is Pool_Worker_Count;

   --  A Plan_Index identifies a worker within the work plan
   type Plan_Worker_Count is new Worker_Count_Type;
   subtype Plan_Index is Plan_Worker_Count;

   --  A Work Plan gives the task pool client the full control on how the
   --  worker manages and approaches its work. The task pool only provides
   --  the workers, the work plan defines the work to be done.
   --
   type Work_Plan is limited interface;

   procedure Engage
     (Plan : in out Work_Plan;
      Worker : Pool_Index;
      Item : Plan_Index) is abstract;
   --  When a worker starts executing, it engages the work plan. The
   --  parallelism manager client defines the work. The Engage is called
   --  once and executes the plan. Upon returning, the Worker is once again
   --  idle and returns to the task pool

   procedure Starting
     (Plan : in out Work_Plan;
      Requester : Plan_Index;
      Item : out Plan_Index) is null;
      --  Routine that gets called before a work plan is engaged, to allow
      --  the plan to initialize any internal state. This routine is
      --  intended to be called from within a protected object associated with
      --  the pool, and therefore must not be potentially blocking

   procedure Completing
     (Plan : in out Work_Plan;
      Item : Plan_Index) is null;
      --  Routine that gets called immediately after work plan is engaged,
      --  to allow the plan to udpate any internal state. This routine is
      --  intended to be called from within a protected object associated with
      --  the pool, and therefore must not be potentially blocking.

   type Task_Pool_Interface is limited interface;

   procedure Offer_Work
     (Pool : in out Task_Pool_Interface;
      Plan : aliased in out Work_Plan'Class;
      Item : Plan_Index) is abstract
   with Pre'Class => Pool.Available_Workers > 0;
   --  Allows a work plan to request workers from the task pool. The Work
   --  plan is offered to the task pool, which is then engaged by each
   --  worker up to the requested Worker_Count
   --  Note: This routine is intended to be called by the parallelism
   --  manager, and not exposed to the user client code

   procedure Offer_Work_To_Group
     (Pool : in out Task_Pool_Interface;
      Plan : aliased in out Work_Plan'Class;
      Worker_Count : Positive_Worker_Count) is abstract
   with Pre'Class => Pool.Available_Workers >= Worker_Count;

   function Available_Workers
     (Pool : Task_Pool_Interface) return Worker_Count_Type is abstract;

   function Total_Workers
     (Pool : Task_Pool_Interface) return Positive_Worker_Count is abstract;

   function Priority
     (Pool : Task_Pool_Interface) return System.Priority is abstract;
   --  Get the priority of the task pool

   procedure Next_Worker_Id
     (Pool : in out Task_Pool_Interface;
      Plan : aliased in out Work_Plan'Class;
      Requester : Plan_Index;
      Item : out Plan_Index) is null;

   procedure Finished_Work
     (Pool : in out Task_Pool_Interface;
      Worker : Pool_Index;
      Plan : aliased in out Work_Plan'Class;
      Item : Plan_Index) is null;

end Parallel.Ravenscar_Task_Pools;
