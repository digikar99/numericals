import time
import numpy as np

def benchmark(*v_lengths):
    timings = []
    for v_len in v_lengths:
        a = np.random.random((v_len))
        b = np.random.random((v_len))
        r = np.zeros((v_len))

        print("Doing ", v_len)
        start = time.time()
        for i in range(int(1e8/v_len)):
            np.add(a, b, out = r)

        timings.append((time.time() - start))
        
    return timings

print(benchmark(10, 100, 1000, 10000, 100000, 1000000, 10000000))
# sizes = [20000*i for i in range(1, 11)]
# print(benchmark(*sizes))
