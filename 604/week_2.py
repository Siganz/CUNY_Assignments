import simpy
import random

random.seed(42)

# Recipe: 8 iron + 3 coke + 1 limestone = 1 final product
IRON_PER_PRODUCT = 8
COKE_PER_PRODUCT = 3
LIMESTONE_PER_PRODUCT = 1

FINAL_PROCESS_TIME = 5

# Total raw material arrivals
IRON_ARRIVALS = 800
COKE_ARRIVALS = 300
LIMESTONE_ARRIVALS = 100

# Mean time between arrivals
IRON_MEAN_ARRIVAL = 2
COKE_MEAN_ARRIVAL = 4
LIMESTONE_MEAN_ARRIVAL = 10

# Buffers: enough for 10 product batches
IRON_BUFFER_CAPACITY = 80
COKE_BUFFER_CAPACITY = 30
LIMESTONE_BUFFER_CAPACITY = 10


def source(env, name, inventory, mean_interarrival, max_arrivals):
    for i in range(max_arrivals):
        yield env.timeout(random.expovariate(1 / mean_interarrival))

        # If inventory is full, this waits until space opens.
        yield inventory.put(1)

        print(f"{env.now:.2f}: {name} arrived | level={inventory.level}")


def furnace(env, iron, coke, limestone):
    product_count = 0

    while True:
        # Wait until the full recipe is available.
        yield iron.get(IRON_PER_PRODUCT)
        yield coke.get(COKE_PER_PRODUCT)
        yield limestone.get(LIMESTONE_PER_PRODUCT)

        print(
            f"{env.now:.2f}: recipe consumed "
            f"| iron={iron.level}, coke={coke.level}, limestone={limestone.level}"
        )

        # Processing time for one final product.
        yield env.timeout(FINAL_PROCESS_TIME)

        product_count += 1
        print(f"{env.now:.2f}: final product #{product_count} completed")


def run():
    env = simpy.Environment()

    iron = simpy.Container(env, init=0, capacity=IRON_BUFFER_CAPACITY)
    coke = simpy.Container(env, init=0, capacity=COKE_BUFFER_CAPACITY)
    limestone = simpy.Container(env, init=0, capacity=LIMESTONE_BUFFER_CAPACITY)

    env.process(source(env, "iron", iron, IRON_MEAN_ARRIVAL, IRON_ARRIVALS))
    env.process(source(env, "coke", coke, COKE_MEAN_ARRIVAL, COKE_ARRIVALS))
    env.process(source(env, "limestone", limestone, LIMESTONE_MEAN_ARRIVAL, LIMESTONE_ARRIVALS))

    env.process(furnace(env, iron, coke, limestone))

    env.run()


if __name__ == "__main__":
    run()