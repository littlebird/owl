package owl.algorithm;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.ArrayDeque;
import java.util.HashMap;

public class Betweenness {
  private Map graph;

  public Betweenness(Map graph) {
    this.graph = graph;
  }

  public Long get(Map<Long, Long> map, Long key, Long base) {
    return map.containsKey(key) ? map.get(key) : base;
  }

  public Double get(Map<Long, Double> map, Long key, Double base) {
    return map.containsKey(key) ? map.get(key) : base;
  }

  public HashMap calculate() {
    int size = this.graph.size();
    HashMap betweenness = new HashMap<Long, Double>(size);

    for (Iterator keys = this.graph.keySet().iterator();
         keys.hasNext();) {
      long node = (long) keys.next();

      ArrayDeque stack = new ArrayDeque<Long> ();
      HashMap<Long, ArrayDeque<Long>> paths = new HashMap(size);
      HashMap<Long, Long> shortest = new HashMap(size);
      HashMap<Long, Long> dependence = new HashMap(size);
      HashMap<Long, Double> distance = new HashMap(size);
      ArrayDeque<Long> queue = new ArrayDeque();
      shortest.put(node, 1l);
      dependence.put(node, 0l);
      queue.add(node);

      while(queue.size() > 0) {
        long visit = (long) queue.poll();
        stack.push(visit);
        Collection neighborList = (Collection) this.graph.get(visit);
        for(Iterator neighbors = neighborList.iterator();
            neighbors.hasNext();) {
          long neighbor = (long) neighbors.next();
          if (!dependence.containsKey(neighbor)) {
            queue.add(neighbor);
            dependence.put(neighbor, this.get(dependence, visit, -1l) + 1);
          }

          if (dependence.get(neighbor) == this.get(dependence, visit, -1l) + 1) {
            long soFar = this.get(shortest, neighbor, 0l);
            long further = this.get(shortest, visit, 1l);
            shortest.put(neighbor, soFar+further);
            if (!paths.containsKey(neighbor)) {
              paths.put(neighbor, new ArrayDeque());
            }
            paths.get(neighbor).add(visit);
          }
        }
      }

      while(!stack.isEmpty()) {
        long last = (long) stack.pop();
        if (paths.containsKey(last)) {
          ArrayDeque pathList = paths.get(last);
          for (Iterator path = pathList.iterator();
               path.hasNext();) {
            long step = (long) path.next();
            double stepDistance = this.get(distance, step, 0.0);
            double ratio = this.get(shortest, step, 0l) / shortest.get(last);
            double scale = this.get(distance, last, 0.0) + 1.0;
            distance.put(step, stepDistance + ratio * scale);
            if (last != node) {
              double lastBetweenness = this.get(betweenness, last, 0.0);
              double lastDistance = this.get(distance, last, 0.0);
              betweenness.put(last,  lastBetweenness + lastDistance);
            }
          }
        }
      }
    }

    return betweenness;
  }
}
