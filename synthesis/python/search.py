from queue import PriorityQueue

from Node import State
from fpga.Location import Location
from fpga.Nets import Net


def reconstruct(previous, current):
    total_path = [current]
    while current in previous:
        current = previous[current]
        total_path.insert(0, current)
    return total_path


def astar(grid, goal: Location, start: Location, signal, blacklist):
    print(f"start: {start}, signal: {signal}, goal: {goal}")
    queue = PriorityQueue()
    iterations = 0
    previous = dict()

    gscore = {start: 0.0}
    fscore = {start: start.distance(goal)}

    queue.put((0.0, start))

    while queue.qsize() > 0:
        if iterations > 50:
            print("DONE 50 iterations")
            return False
        iterations += 1

        score, current = queue.get()

        shared_luts = grid.find_shared_lut_inputs(current, goal)
        if len(shared_luts) > 0 and shared_luts.pop().states == [State.DEFAULT, State.DEFAULT]:
            return reconstruct(previous, current)

        for neighbour in grid.connections(current):

            if neighbour.x < 0 or neighbour.y < 0:
                continue


            # Check if neighbour is already our signal or none signal
            net = grid.getNet(neighbour)
            if (net.assignment is None) and neighbour not in blacklist:
                tentative_gScore = gscore[current] + 1
                if neighbour not in gscore or gscore[neighbour] > tentative_gScore:
                    previous[neighbour] = current
                    gscore[neighbour] = tentative_gScore
                    queue.put((tentative_gScore + neighbour.distance(goal), neighbour))


def astar2(grid, goal: Net, start: Net):
    queue = PriorityQueue()
    iterations = 0
    previous = dict()

    acceptable_luts = [o.parent for o in goal.outputs if o.parent.states == [State.DEFAULT, State.DEFAULT]]
    g_score = {x: 0.0 for x in start.inputs}

    for i in start.inputs:
        queue.put((0.0, i))

    while queue.qsize() > 0:
        if iterations > 50:
            return False
        iterations += 1

        score, current = queue.get()
        lut = current.parent
        if lut in acceptable_luts:
            return reconstruct(previous, current)

        if lut.state != [State.DEFAULT, State.DEFAULT]:
            continue

        for output in lut.outputs:
            net = grid.getNet(output.tile.location)
            if net.assignment is not None:
                continue

            for i in net.inputs:
                tentative_g_score = g_score[current] + 1
                previous[i] = current
                g_score[i] = tentative_g_score
                queue.put((tentative_g_score + i.location.distance(goal.location)))




