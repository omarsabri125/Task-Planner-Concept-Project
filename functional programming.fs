open System
open System.IO
open Newtonsoft.Json

type status =
    | Pending = 1
    | InProgress = 2
    | Completed = 3
    | Overdue = 4
// Define the Task type
type Task = {
    TaskID: int
    Description: string
    DueDate: DateOnly
    Priority: int
    Status: status
    CreationDate: DateOnly
}

// Function to create a new task with a unique ID and the current date as the due date
let createTask tasks description dueDate priority status = 
    let newId = 
        if List.isEmpty tasks then 1
        else (tasks |> List.maxBy (fun task -> task.TaskID)).TaskID + 1
    // let dueDate = DateTime.Now
    let creationDate = DateOnly.FromDateTime(DateTime.Now)
    { TaskID = newId; Description = description; DueDate = dueDate; Priority = priority; Status = status; CreationDate = creationDate }

// Function to update a task's status
let updateTaskStatus task newStatus = 
    { task with Status = newStatus }

// Function to update a task's priority
let updateTaskPriority task newPriority = 
    { task with Priority = newPriority }

// Function to delete a task from the list
let deleteTask tasks taskId = 
    tasks |> List.filter (fun task -> task.TaskID <> taskId)

// Function to filter tasks based on criteria
let filterTasks tasks criteria = 
    tasks |> List.filter criteria

// Function to sort tasks by due date
let sortTasksByDueDate tasks = 
    tasks |> List.sortBy (fun task -> task.DueDate)

// Function to sort tasks by priority
let sortTasksByPriority tasks = 
    tasks |> List.sortBy (fun task -> task.Priority)

// Function to highlight tasks nearing deadlines
let highlightNearingDeadlines tasks daysBefore = 
    let today = DateOnly.FromDateTime(DateTime.Now)
    tasks 
    |> List.filter (fun task -> 
        let dueSoon = task.DueDate >= today && task.DueDate <= today.AddDays(daysBefore)
        dueSoon && (task.Status = status.Pending || task.Status = status.InProgress))

// Function to notify users of overdue tasks
let notifyOverdueTasks tasks = 
    let now = DateOnly.FromDateTime(DateTime.Now)
    tasks |> List.filter (fun task -> task.DueDate < now)

// Function to save tasks to a file
let saveTasksToFile tasks filePath = 
    let json = JsonConvert.SerializeObject(tasks, Formatting.Indented)
    File.WriteAllText(filePath, json)

// Function to load tasks from a file
let loadTasksFromFile filePath = 
    let json = File.ReadAllText(filePath)
    JsonConvert.DeserializeObject<Task list>(json)

// Function to display tasks
let displayTasks tasks =
    // Print table header
    printfn "%-5s %-20s %-12s %-10s %-10s %-20s" "ID" "Description" "Due Date" "Priority" "Status" "Creation Date"
    printfn "%s" (String.replicate 80 "-") // Horizontal line for separation

    // Print each task as a row
    tasks |> List.iter (fun task -> 
        printfn "%-5d %-20s %-12s %-10d %-10s %-20s" 
            task.TaskID 
            task.Description 
            (task.DueDate.ToString("yyyy-MM-dd")) // Format date to exclude time
            task.Priority 
            (string task.Status)
            (task.CreationDate.ToString("yyyy-MM-dd")))

// Function to Prompt For Date
let rec promptForDate () =
    printf "Enter Due Date (yyyy-MM-dd): "
    let input = Console.ReadLine()
    match DateOnly.TryParseExact(input, "yyyy-MM-dd", null, Globalization.DateTimeStyles.None) with
    | (true, parsedDate) -> parsedDate // Valid date, return it
    | (false, _) -> 
        printfn "Invalid date format. Please try again."
        promptForDate() // Retry if invalid

// Main interactive loop
let rec mainLoop tasks =
    printfn "\nChoose an option: "
    printfn "1. Add Task"
    printfn "2. Update Task"
    printfn "3. Delete Task"
    printfn "4. Display Tasks"
    printfn "5. Sort Tasks by Due Date"
    printfn "6. Sort Tasks by Priority"
    printfn "7. Highlight Tasks Nearing Deadlines"
    printfn "8. Notify Overdue Tasks"
    printfn "9. Save and Exit"
    let choice = Console.ReadLine()
    match choice with
    | "1" ->
        printf "Enter Description: "
        let description = Console.ReadLine()

        let dueDate = promptForDate()

        printf "Enter Priority: "
        let priority = int (Console.ReadLine())

        printfn "Choose Status: "
        printfn "1) Pending"
        printfn "2) In Progress"
        printfn "3) Completed"
        printfn "4) Overdue"
        let status = enum (int (Console.ReadLine()))

        let newTask = createTask tasks description dueDate priority status
        mainLoop (newTask :: tasks)
    | "2" ->
        printf "Enter Task ID to update: "
        let id = int (Console.ReadLine())
        let taskToUpdate = tasks |> List.find (fun task -> task.TaskID = id)

        printf "Enter New Priority: "
        let newPriority = int (Console.ReadLine())

        printfn "Choose new Status: "
        printfn "1) Pending"
        printfn "2) In Progress"
        printfn "3) Completed"
        printfn "4) Overdue"
        let newStatus = enum (int (Console.ReadLine())) 

        let updatedTaskv1 = updateTaskStatus taskToUpdate newStatus
        let updatedTaskv2 = updateTaskPriority updatedTaskv1 newPriority 
        let updatedTasks = updatedTaskv2 :: (tasks |> List.filter (fun task -> task.TaskID <> id))
        mainLoop updatedTasks
    | "3" ->
        printf "Enter Task ID to delete: "
        let id = int (Console.ReadLine())
        let updatedTasks = deleteTask tasks id
        mainLoop updatedTasks
    | "4" ->
        displayTasks tasks
        mainLoop tasks
    | "5" ->
        let sortedTasks = sortTasksByDueDate tasks
        displayTasks sortedTasks
        mainLoop sortedTasks
    | "6" ->
        let sortedTasks = sortTasksByPriority tasks
        displayTasks sortedTasks
        mainLoop sortedTasks
    | "7" ->
        printf "Enter number of days before deadline to highlight: "
        let daysBefore = int (Console.ReadLine())
        let nearingDeadlines = highlightNearingDeadlines tasks daysBefore
        printfn "Tasks nearing deadlines:"
        displayTasks nearingDeadlines
        mainLoop tasks
    | "8" ->
        let overdueTasks = notifyOverdueTasks tasks
        printfn "Overdue Tasks:"
        displayTasks overdueTasks
        mainLoop tasks
    | "9" ->
        saveTasksToFile tasks "tasks.json"
        printfn "Tasks saved. Exiting..."
    | _ ->
        printfn "Invalid choice. Try again."
        mainLoop tasks

// Load existing tasks or start with an empty list
let tasks = 
    if File.Exists("tasks.json") then
        loadTasksFromFile "tasks.json"
    else
        []

// Start the main loop
mainLoop tasks