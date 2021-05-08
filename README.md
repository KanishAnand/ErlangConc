# Erlang Parallel Programming

## Problem 1

Write a program to pass an integer token value around all processes in a ring-like fashion, and make sure that it does not have a deadlock.

### Steps to run the code

1. erlc 2018101025_1.erl
2. erl -noshell -s 2018101025_1 main <input_file> <output_file> -s init stop

### Implementation

- N processes are created where N is the no. of processes.
- Each process i from 1 to (N - 1) is sent Pid of process (i + 1) % N to connect them in ring fashion.
- Process 0 sends token to process 1.
- Each process from 1 to (N - 1) receives token and sends token to next process in ring fashion.
- Finally process 0 receives the token back.

### Complexity Analysis

Message Complexity: N, Each process sends one msg to its next process. So toal N msgs are sent.

Time Complexity: O(N)

## Problem 2

Given a non-negative weighted, undirected, connected graph and a source vertex in the graph, find the shortest paths from source to all vertices in the graph. You can use any algorithm to solve this problem.

### Steps to run the code

1. erlc 2018101025_2.erl
2. erl -noshell -s 2018101025_2 main <input_file> <output_file> -s init stop

### Implementation

Parallel Bellman Ford Algorithm is implemented to find shortest path from source. The basic strategy involves dividing the edges amongst all the processes in such a manner that each process relaxes the edges independently in the outer iteration (Vertices -1 times) and after each iteration, all the respective distance arrays of each process are merged.

- P processes are created where P is the no. of processes and V is the no. of vertices.
- Edge list is distributed among processes equally.
- Bellman Ford algo: for 1 to (V - 1) -> relax all the edges and update Distance array. After (V - 1) loops, we get final Distance array.
- Here, for 1 to (V - 1) -> Main process shares Distance array with every process. Every process then relaxes their edges and returns new Distance array to Root process. Root process gets P Distance array's.
- Root process then merges all recieved distance arrays by taking minimum of distance of each node and prepares final distance array.
- This is done V-1 times and the final array is stored in output file.

### Complexity Analysis

Message Complexity: (P)\*2(V-1) or O(PV),
where (V - 1) times message is sent to P processes and P processes send message back to root process.

Time Complexity: O(VE/P) + O(PV) \* Avg message time.
Bellman Ford algo has O(VE) time complexity, but in parallel version E is divides among P processes working in parallel.
