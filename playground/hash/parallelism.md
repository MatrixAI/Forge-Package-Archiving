There are 3 types of parallelism: data parallelism, task parallelism, and pipeline parallelism
Data parallelism refers to running the same task on different data, and task vice versa.
Pipeline parallelism refers to running the same task composed of multiple orders at the same time on different cores.

external paralleism can be used for pipeline parallelism of multiple dependent threads.

internal parallelism is better suited to running parallel hashing.
with the internal parallelism model, you can minimise context switches by letting each hash thread run ahead of each other, and having them sleep at the very end. However, this increases complexity for only a small decrease in overhead, so for the moment, just have each hash wait on each other to finish before fetching the next bytestring.

In any case, you have to wait for the slowest thread to finish anyway, so the total time shouldn't matter...
